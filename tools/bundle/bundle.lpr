program bundle;

{$mode delphi}

{ Bundle makes the application bundle of a program built on macOS. It is run
  by the Execute after command of a project, after the program was compiled.

    bundle <mode> [--dat] <program>

  Mode is the name of the build mode, which the command gives with
  $(BuildMode). A name with release in it, in any case, makes a release.
  Any other name, such as Default or Debug, makes a debug bundle. --dat is
  only used by a release.

  On other systems bundle does nothing and ends with zero, so a project made
  on macOS still builds there.

  The bundle is <program>.app beside the program. It is made if it is not
  there yet.

  A release makes the bundle stand by itself:

    The program is copied to Contents/MacOS.

    The libraries the program loads with @rpath are found in the folders of
    its rpaths and copied to Contents/Frameworks. These are the libraries it
    is linked with, the libraries named in it which it loads while it runs,
    and the same for each library which is copied.

    The only rpath left in the program is @executable_path/../Frameworks.
    The libraries lose every rpath which is not inside the bundle, and are
    given @loader_path if they load other libraries with @rpath.

    With --dat the program is started with --build-dat. It finds the assets
    folder above the bundle and writes its dat file, which is moved to
    Contents/Resources. Without --dat the assets folder beside the program
    is copied to Contents/Resources. Either way a link beside the program in
    Contents/MacOS points to them, so the program finds them beside itself.

    The icon is <program>.icns, or one made from <program>.png, beside the
    program.

    The libraries and then the bundle are signed for this computer.

  A debug bundle has what a release left in it removed, and its program is a
  link to the program again.

  The exit code is zero if it worked and one if not. }

uses
  SysUtils, Classes, StrUtils, Process, BaseUnix;

const
  BundleExt = '.app';
  FrameworksRPath = '@executable_path/../Frameworks';
  LoaderRPath = '@loader_path';
  RPathPrefix = '@rpath/';
  LibraryExt = '.dylib';
  AssetsName = 'assets';
  DatExt = '.dat';
  DatSwitch = '--build-dat';
  ToolSeconds = 60;
  DatSeconds = 300;

type
  EBundleError = class(Exception);

var
  { The program which was built, its bundle, and the folders of its bundle.
    The folders end with a separator. }
  ProgramFile: string;
  BundleDir: string;
  ContentsDir: string;
  MacOSDir: string;
  FrameworksDir: string;
  ResourcesDir: string;
  { The program inside the bundle }
  BundleExe: string;

procedure RaiseError(const S: string);
begin
  raise EBundleError.Create(S);
end;

procedure Warn(const S: string);
begin
  WriteLn('Warning: ', S);
end;

{ Run a tool and wait for it, giving what it wrote. False if it could not be
  started, did not end in time, or ended with an error. }

function RunTool(const Exe: string; const Params: array of string;
  Seconds: Integer; out Output: string): Boolean;
var
  Proc: TProcess;
  Buf: array[0..4095] of Char;
  Start: QWord;
  Count, I: Integer;
  TimedOut: Boolean;

  procedure ReadOutput;
  begin
    while Proc.Output.NumBytesAvailable > 0 do
    begin
      Count := Proc.Output.Read(Buf[0], SizeOf(Buf));
      if Count <= 0 then
        Break;
      SetLength(Output, Length(Output) + Count);
      Move(Buf[0], Output[Length(Output) - Count + 1], Count);
    end;
  end;

begin
  Result := False;
  Output := '';
  TimedOut := False;
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Exe;
    for I := Low(Params) to High(Params) do
      Proc.Parameters.Add(Params[I]);
    Proc.Options := [poUsePipes, poStderrToOutPut];
    try
      Proc.Execute;
    except
      on E: Exception do
      begin
        Output := E.Message;
        Exit;
      end;
    end;
    Start := GetTickCount64;
    while Proc.Running do
    begin
      ReadOutput;
      if GetTickCount64 - Start > QWord(Seconds) * 1000 then
      begin
        TimedOut := True;
        Proc.Terminate(1);
        Break;
      end;
      Sleep(10);
    end;
    ReadOutput;
    if TimedOut then
      Output := Output + LineEnding + 'It did not end after ' + IntToStr(Seconds) + ' seconds'
    else
      Result := Proc.ExitCode = 0;
  finally
    Proc.Free;
  end;
end;

function CommandText(const Exe: string; const Params: array of string): string;
var
  I: Integer;
begin
  Result := Exe;
  for I := Low(Params) to High(Params) do
    Result := Result + ' ' + Params[I];
end;

{ Run a tool, failing if it does not work }

procedure Run(const Exe: string; const Params: array of string);
var
  Output: string;
begin
  if not RunTool(Exe, Params, ToolSeconds, Output) then
    RaiseError('"' + CommandText(Exe, Params) + '" failed: ' + Trim(Output));
end;

{ Run a tool, giving a warning if it does not work }

procedure RunWarn(const Exe: string; const Params: array of string);
var
  Output: string;
begin
  if not RunTool(Exe, Params, ToolSeconds, Output) then
    Warn('"' + CommandText(Exe, Params) + '" failed: ' + Trim(Output));
end;

{ Files and folders. A link is handled as a link, never as what it points to. }

function PathExists(const Path: string): Boolean;
var
  Info: Stat;
begin
  Result := fpLstat(PChar(ExcludeTrailingPathDelimiter(Path)), @Info) = 0;
end;

function IsLink(const Path: string): Boolean;
var
  Info: Stat;
begin
  Result := (fpLstat(PChar(ExcludeTrailingPathDelimiter(Path)), @Info) = 0)
    and fpS_ISLNK(Info.st_mode);
end;

procedure ForceFolder(const Folder: string);
begin
  if not ForceDirectories(Folder) then
    RaiseError('Unable to make the folder "' + Folder + '"');
end;

{ Remove a file, a link, or a folder with everything in it }

procedure RemovePath(const Path: string);
var
  Name, Folder: string;
  Info: Stat;
  Search: TSearchRec;
begin
  Name := ExcludeTrailingPathDelimiter(Path);
  if fpLstat(PChar(Name), @Info) <> 0 then
    Exit;
  if fpS_ISDIR(Info.st_mode) then
  begin
    Folder := IncludeTrailingPathDelimiter(Name);
    if FindFirst(Folder + '*', faAnyFile, Search) = 0 then
    try
      repeat
        if (Search.Name <> '.') and (Search.Name <> '..') then
          RemovePath(Folder + Search.Name);
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
    if not RemoveDir(Name) then
      RaiseError('Unable to remove the folder "' + Name + '"');
  end
  else if fpUnlink(PChar(Name)) <> 0 then
    RaiseError('Unable to remove "' + Name + '"');
end;

procedure CopyFile(const Source, Dest: string);
var
  S, D: TFileStream;
begin
  ForceFolder(ExtractFilePath(Dest));
  S := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    D := TFileStream.Create(Dest, fmCreate);
    try
      if S.Size > 0 then
        D.CopyFrom(S, S.Size);
    finally
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

{ Copy the files of a folder and the folders inside it. Names which begin
  with a period are left out, as they are when a dat file is built. }

procedure CopyFolder(const Source, Dest: string);
var
  Search: TSearchRec;
  S, D: string;
begin
  S := IncludeTrailingPathDelimiter(Source);
  D := IncludeTrailingPathDelimiter(Dest);
  ForceFolder(D);
  if FindFirst(S + '*', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name = '') or (Search.Name[1] = '.') then
        Continue;
      if Search.Attr and faDirectory <> 0 then
        CopyFolder(S + Search.Name, D + Search.Name)
      else
        CopyFile(S + Search.Name, D + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

procedure MakeLink(const Target, LinkName: string);
begin
  if fpSymlink(PChar(Target), PChar(LinkName)) <> 0 then
    RaiseError('Unable to make the link "' + LinkName + '" to "' + Target + '"');
end;

procedure MakeExecutable(const FileName: string);
begin
  fpChmod(PChar(FileName), &755);
end;

function ReadText(const FileName: string): string;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure WriteText(const FileName, Text: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Text <> '' then
      Stream.WriteBuffer(Text[1], Length(Text));
  finally
    Stream.Free;
  end;
end;

{ What otool says about a program or a library }

{ The rpaths of a file, in the order they are stored }

procedure GetRPaths(const FileName: string; RPaths: TStrings);
var
  Output, Line: string;
  Lines: TStringList;
  InRPath: Boolean;
  I, P: Integer;
begin
  RPaths.Clear;
  if not RunTool('/usr/bin/otool', ['-l', FileName], ToolSeconds, Output) then
    RaiseError('Unable to read the rpaths of "' + FileName + '": ' + Trim(Output));
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    InRPath := False;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Copy(Line, 1, 4) = 'cmd ' then
        InRPath := Line = 'cmd LC_RPATH'
      else if InRPath and (Copy(Line, 1, 5) = 'path ') then
      begin
        Delete(Line, 1, 5);
        P := Pos(' (offset', Line);
        if P > 0 then
          Delete(Line, P, Length(Line));
        RPaths.Add(Line);
        InRPath := False;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ The name of a library which other files load it by, or nothing }

function GetInstallName(const FileName: string): string;
var
  Output: string;
  Lines: TStringList;
begin
  Result := '';
  if not RunTool('/usr/bin/otool', ['-D', FileName], ToolSeconds, Output) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    { The first line is the name of the file }
    if Lines.Count > 1 then
      Result := Trim(Lines[Lines.Count - 1]);
  finally
    Lines.Free;
  end;
end;

{ The names of the libraries a file is linked with which are loaded with
  @rpath, without the @rpath. Its own name is left out. }

procedure GetLinkedLibraries(const FileName: string; Names: TStrings);
var
  Output, Line, Own: string;
  Lines: TStringList;
  I, P: Integer;
begin
  Names.Clear;
  if not RunTool('/usr/bin/otool', ['-L', FileName], ToolSeconds, Output) then
    RaiseError('Unable to read the libraries of "' + FileName + '": ' + Trim(Output));
  Own := GetInstallName(FileName);
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    { The first line is the name of the file }
    for I := 1 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      P := Pos(' (', Line);
      if P > 0 then
        Delete(Line, P, Length(Line));
      if (Line = Own) or (Copy(Line, 1, Length(RPathPrefix)) <> RPathPrefix) then
        Continue;
      Delete(Line, 1, Length(RPathPrefix));
      if (Line <> '') and (Names.IndexOf(Line) < 0) then
        Names.Add(Line);
    end;
  finally
    Lines.Free;
  end;
end;

{ The names of the libraries written in a file, which it may load while it
  runs: each text which ends with .dylib, without the folder before it }

procedure GetNamedLibraries(const FileName: string; Names: TStrings);
var
  Content, Name: string;
  P, Start, Finish: Integer;
begin
  Names.Clear;
  Content := ReadText(FileName);
  P := Pos(LibraryExt, Content);
  while P > 0 do
  begin
    Finish := P + Length(LibraryExt) - 1;
    Start := P;
    while (Start > 1) and (Content[Start - 1] > ' ') and (Content[Start - 1] < #127)
      and (Content[Start - 1] <> '/') do
      Dec(Start);
    Name := Copy(Content, Start, Finish - Start + 1);
    if (Name <> LibraryExt) and (Names.IndexOf(Name) < 0) then
      Names.Add(Name);
    P := PosEx(LibraryExt, Content, Finish + 1);
  end;
end;

{ The bundle }

{ Make the folders, Info.plist and PkgInfo of the bundle if it is not there }

procedure MakeBundle;
var
  Name, Plist: string;
begin
  ForceFolder(MacOSDir);
  ForceFolder(ResourcesDir);
  Name := ExtractFileName(BundleExe);
  Plist := ContentsDir + 'Info.plist';
  if not FileExists(Plist) then
    WriteText(Plist,
      '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
      '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' + LineEnding +
      '<plist version="1.0">' + LineEnding +
      '<dict>' + LineEnding +
      '  <key>CFBundleDevelopmentRegion</key>' + LineEnding +
      '  <string>English</string>' + LineEnding +
      '  <key>CFBundleExecutable</key>' + LineEnding +
      '  <string>' + Name + '</string>' + LineEnding +
      '  <key>CFBundleName</key>' + LineEnding +
      '  <string>' + Name + '</string>' + LineEnding +
      '  <key>CFBundleIdentifier</key>' + LineEnding +
      '  <string>com.company.' + Name + '</string>' + LineEnding +
      '  <key>CFBundleInfoDictionaryVersion</key>' + LineEnding +
      '  <string>6.0</string>' + LineEnding +
      '  <key>CFBundlePackageType</key>' + LineEnding +
      '  <string>APPL</string>' + LineEnding +
      '  <key>CFBundleSignature</key>' + LineEnding +
      '  <string>' + Copy(Name + '????', 1, 4) + '</string>' + LineEnding +
      '  <key>CFBundleShortVersionString</key>' + LineEnding +
      '  <string>0.1</string>' + LineEnding +
      '  <key>CFBundleVersion</key>' + LineEnding +
      '  <string>1</string>' + LineEnding +
      '  <key>CSResourcesFileMapped</key>' + LineEnding +
      '  <true/>' + LineEnding +
      '  <key>NSHighResolutionCapable</key>' + LineEnding +
      '  <true/>' + LineEnding +
      '</dict>' + LineEnding +
      '</plist>' + LineEnding);
  if not FileExists(ContentsDir + 'PkgInfo') then
    WriteText(ContentsDir + 'PkgInfo', 'APPL????');
end;

{ Remove what a release placed in the bundle, and the program of the bundle,
  whether it is a link or a copy }

procedure CleanBundle;
var
  DatName: string;
begin
  DatName := ExtractFileName(ChangeFileExt(BundleExe, DatExt));
  RemovePath(FrameworksDir);
  RemovePath(MacOSDir + AssetsName);
  RemovePath(MacOSDir + DatName);
  RemovePath(ResourcesDir + AssetsName);
  RemovePath(ResourcesDir + DatName);
  RemovePath(BundleExe);
end;

{ Give the bundle the icon <program>.icns, or one made from <program>.png,
  which are beside the program. A failure is only a warning. }

procedure SetIcon;
const
  { The sizes of an icon set, each also having a retina version twice as large }
  Sizes: array[0..4] of Integer = (16, 32, 128, 256, 512);
var
  Name, Source, Dest, Plist, IconSet, Output, Size: string;
  Width, I: Integer;
  Made: Boolean;

  procedure Scale(Pixels: Integer; const IconName: string);
  begin
    if Pixels > Width then
      Exit;
    if not RunTool('/usr/bin/sips', ['-z', IntToStr(Pixels), IntToStr(Pixels),
      Source, '--out', IconSet + IconName + '.png'], ToolSeconds, Output) then
      Made := False;
  end;

begin
  Name := ExtractFileName(BundleExe);
  Dest := ResourcesDir + Name + '.icns';
  Source := ChangeFileExt(ProgramFile, '.icns');
  if FileExists(Source) then
  begin
    if (not FileExists(Dest)) or (FileAge(Dest) < FileAge(Source)) then
    try
      CopyFile(Source, Dest);
    except
      on E: Exception do
      begin
        Warn('Unable to copy "' + Source + '" to "' + Dest + '": ' + E.Message);
        Exit;
      end;
    end;
  end
  else
  begin
    Source := ChangeFileExt(ProgramFile, '.png');
    if not FileExists(Source) then
      Exit;
    if (not FileExists(Dest)) or (FileAge(Dest) < FileAge(Source)) then
    begin
      Width := 0;
      if RunTool('/usr/bin/sips', ['-g', 'pixelWidth', Source], ToolSeconds, Output) then
        Width := StrToIntDef(Trim(Copy(Output, Pos('pixelWidth:', Output) + 11, 12)), 0);
      if Width < 16 then
      begin
        Warn('Unable to read the size of "' + Source + '"');
        Exit;
      end;
      IconSet := IncludeTrailingPathDelimiter(GetTempDir(False)) + Name + '.iconset' + PathDelim;
      RemovePath(IconSet);
      ForceFolder(IconSet);
      Made := True;
      for I := Low(Sizes) to High(Sizes) do
      begin
        Size := IntToStr(Sizes[I]) + 'x' + IntToStr(Sizes[I]);
        Scale(Sizes[I], 'icon_' + Size);
        Scale(Sizes[I] * 2, 'icon_' + Size + '@2x');
      end;
      Made := Made and RunTool('/usr/bin/iconutil', ['-c', 'icns', '-o', Dest,
        ExcludeTrailingPathDelimiter(IconSet)], ToolSeconds, Output);
      RemovePath(IconSet);
      if not Made then
      begin
        Warn('Unable to make "' + Dest + '": ' + Trim(Output));
        Exit;
      end;
    end;
  end;
  { Name the icon in Info.plist and have the Finder show it }
  Plist := ContentsDir + 'Info.plist';
  if not FileExists(Plist) then
    Exit;
  if Pos('CFBundleIconFile', ReadText(Plist)) > 0 then
    Exit;
  RunWarn('/usr/bin/plutil', ['-replace', 'CFBundleIconFile', '-string', Name, Plist]);
  RunWarn('/usr/bin/touch', [ExcludeTrailingPathDelimiter(BundleDir)]);
end;

{ Debug: the program of the bundle is a link to the program }

procedure MakeDebug;
begin
  MakeBundle;
  if not IsLink(BundleExe) then
  begin
    CleanBundle;
    MakeLink('..' + PathDelim + '..' + PathDelim + '..' + PathDelim +
      ExtractFileName(ProgramFile), BundleExe);
  end;
  SetIcon;
  WriteLn('The program of ', ExcludeTrailingPathDelimiter(BundleDir), ' is a link to ', ProgramFile);
end;

{ Release: the bundle stands by itself }

procedure MakeRelease(BuildDat: Boolean);
var
  SearchDirs, Copied, Sources, RPaths, Names: TStringList;
  Dir, Output, DatFile, DatName, AssetsDir: string;
  I, J, Done: Integer;

  { Add the folders of the rpaths of a file to the folders searched for
    libraries. Loader is the folder of the file. }

  procedure AddSearchDirs(const FileName, Loader: string);
  var
    K: Integer;
  begin
    GetRPaths(FileName, RPaths);
    for K := 0 to RPaths.Count - 1 do
    begin
      if RPaths[K] = FrameworksRPath then
        Continue;
      Dir := StringReplace(RPaths[K], '@executable_path', ExtractFileDir(ProgramFile), []);
      Dir := StringReplace(Dir, '@loader_path', Loader, []);
      if (Dir = '') or (Dir[1] <> '/') then
        Continue;
      Dir := IncludeTrailingPathDelimiter(ExpandFileName(Dir));
      if DirectoryExists(Dir) and (SearchDirs.IndexOf(Dir) < 0) then
        SearchDirs.Add(Dir);
    end;
  end;

  { Copy a library into the bundle from the first folder which holds it.
    The file a link points to is copied with the name it is loaded by. }

  procedure CopyLibrary(const LibName: string; Required: Boolean);
  var
    K: Integer;
    Source, Dest: string;
  begin
    if Copied.IndexOf(LibName) > -1 then
      Exit;
    Source := '';
    for K := 0 to SearchDirs.Count - 1 do
      if FileExists(SearchDirs[K] + LibName) then
      begin
        Source := SearchDirs[K] + LibName;
        Break;
      end;
    if Source = '' then
    begin
      if Required then
        RaiseError('The library "' + LibName + '" was not found in the rpaths of "' + ProgramFile + '"');
      Exit;
    end;
    Dest := FrameworksDir + LibName;
    CopyFile(Source, Dest);
    MakeExecutable(Dest);
    Copied.Add(LibName);
    Sources.Add(Source);
  end;

begin
  MakeBundle;
  CleanBundle;
  SearchDirs := TStringList.Create;
  Copied := TStringList.Create;
  Sources := TStringList.Create;
  RPaths := TStringList.Create;
  Names := TStringList.Create;
  try
    { The program }
    CopyFile(ProgramFile, BundleExe);
    MakeExecutable(BundleExe);
    AddSearchDirs(ProgramFile, ExtractFileDir(ProgramFile));

    { The libraries it is linked with and the libraries named in it }
    GetLinkedLibraries(ProgramFile, Names);
    for I := 0 to Names.Count - 1 do
      CopyLibrary(Names[I], True);
    GetNamedLibraries(ProgramFile, Names);
    for I := 0 to Names.Count - 1 do
      CopyLibrary(Names[I], False);

    { The same for each library which was copied. A library is also looked
      for in the rpaths of the library which loads it. }
    Done := 0;
    while Done < Copied.Count do
    begin
      AddSearchDirs(Sources[Done], ExtractFileDir(Sources[Done]));
      GetLinkedLibraries(Sources[Done], Names);
      for I := 0 to Names.Count - 1 do
        CopyLibrary(Names[I], True);
      GetNamedLibraries(Sources[Done], Names);
      for I := 0 to Names.Count - 1 do
        CopyLibrary(Names[I], False);
      Inc(Done);
    end;

    { The bundle is the only place the program looks for its libraries }
    GetRPaths(BundleExe, RPaths);
    for I := 0 to RPaths.Count - 1 do
      if RPaths[I] <> FrameworksRPath then
        Run('/usr/bin/install_name_tool', ['-delete_rpath', RPaths[I], BundleExe]);
    if RPaths.IndexOf(FrameworksRPath) < 0 then
      Run('/usr/bin/install_name_tool', ['-add_rpath', FrameworksRPath, BundleExe]);

    { The libraries keep only rpaths inside the bundle }
    for I := 0 to Copied.Count - 1 do
    begin
      Dir := FrameworksDir + Copied[I];
      GetRPaths(Dir, RPaths);
      for J := 0 to RPaths.Count - 1 do
        if (RPaths[J] <> LoaderRPath) and (RPaths[J] <> FrameworksRPath) then
          Run('/usr/bin/install_name_tool', ['-delete_rpath', RPaths[J], Dir]);
      GetLinkedLibraries(Dir, Names);
      if (Names.Count > 0) and (RPaths.IndexOf(LoaderRPath) < 0) then
        Run('/usr/bin/install_name_tool', ['-add_rpath', LoaderRPath, Dir]);
    end;

    { Changing the rpaths made the signatures invalid. The libraries and the
      program are signed before the program is started to build the dat
      file. A program which is not signed still runs on the computer it was
      built on, so a failure is only a warning. }
    for I := 0 to Copied.Count - 1 do
      RunWarn('/usr/bin/codesign', ['--force', '--sign', '-', FrameworksDir + Copied[I]]);
    RunWarn('/usr/bin/codesign', ['--force', '--sign', '-', BundleExe]);

    { The assets }
    if BuildDat then
    begin
      { The program finds the assets folder above the bundle and writes the
        dat file beside itself }
      if not RunTool(BundleExe, [DatSwitch], DatSeconds, Output) then
        RaiseError('"' + BundleExe + ' ' + DatSwitch + '" failed: ' + Trim(Output));
      DatFile := ChangeFileExt(BundleExe, DatExt);
      if not FileExists(DatFile) then
        RaiseError('"' + BundleExe + ' ' + DatSwitch + '" did not write "' + DatFile + '": ' + Trim(Output));
      DatName := ExtractFileName(DatFile);
      if not RenameFile(DatFile, ResourcesDir + DatName) then
        RaiseError('Unable to move "' + DatFile + '" to "' + ResourcesDir + DatName + '"');
      MakeLink('..' + PathDelim + 'Resources' + PathDelim + DatName, MacOSDir + DatName);
    end
    else
    begin
      AssetsDir := ExtractFilePath(ProgramFile) + AssetsName;
      if DirectoryExists(AssetsDir) then
      begin
        CopyFolder(AssetsDir, ResourcesDir + AssetsName);
        MakeLink('..' + PathDelim + 'Resources' + PathDelim + AssetsName, MacOSDir + AssetsName);
      end;
    end;

    SetIcon;

    { The bundle is signed last, once everything in it is in place }
    RunWarn('/usr/bin/codesign', ['--force', '--sign', '-', ExcludeTrailingPathDelimiter(BundleDir)]);

    WriteLn('Made ', ExcludeTrailingPathDelimiter(BundleDir), ' with ', Copied.Count, ' libraries');
    for I := 0 to Copied.Count - 1 do
      WriteLn('  ', Copied[I], ' from ', ExtractFilePath(Sources[I]));
  finally
    Names.Free;
    RPaths.Free;
    Sources.Free;
    Copied.Free;
    SearchDirs.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('Usage: bundle <mode> [--dat] <program>');
  WriteLn('  A mode with release in its name makes a release, any other a debug bundle');
  ExitCode := 1;
end;

var
  Mode: string;
  BuildDat: Boolean;
  I: Integer;
begin
  {$ifndef darwin}
  { Only macOS has application bundles }
  Exit;
  {$endif}
  Mode := '';
  ProgramFile := '';
  BuildDat := False;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--dat' then
      BuildDat := True
    else if Mode = '' then
      Mode := ParamStr(I)
    else if ProgramFile = '' then
      ProgramFile := ParamStr(I)
    else
    begin
      Usage;
      Exit;
    end;
  if (Mode = '') or (ProgramFile = '') then
  begin
    Usage;
    Exit;
  end;
  try
    ProgramFile := ExpandFileName(ProgramFile);
    if not FileExists(ProgramFile) then
      RaiseError('The program "' + ProgramFile + '" was not found');
    BundleDir := ChangeFileExt(ProgramFile, '') + BundleExt + PathDelim;
    ContentsDir := BundleDir + 'Contents' + PathDelim;
    MacOSDir := ContentsDir + 'MacOS' + PathDelim;
    FrameworksDir := ContentsDir + 'Frameworks' + PathDelim;
    ResourcesDir := ContentsDir + 'Resources' + PathDelim;
    BundleExe := MacOSDir + ExtractFileName(ChangeFileExt(ProgramFile, ''));
    if Pos('release', LowerCase(Mode)) > 0 then
      MakeRelease(BuildDat)
    else
      MakeDebug;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

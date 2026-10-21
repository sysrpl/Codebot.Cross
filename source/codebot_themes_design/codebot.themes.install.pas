(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Install loads the themes of the IDE and remembers the one
  chosen, and downloads themes and color schemes of the source editor from
  links, installing and removing them while the IDE runs }

{ <include docs/codebot.themes.install.txt> }
unit Codebot.Themes.Install;

{$mode delphi}

interface

{ Fills ThemeList with System Default and the themes the user installed }
procedure ThemesReload;
{ The name of the theme saved by ThemesSaveName, or the name of the default
  theme if none was saved }
function ThemesLoadName: string;
{ Saves the name of the theme in use to the configuration of the IDE }
procedure ThemesSaveName;

{ TThemeDownload is a theme or a color scheme which was downloaded and can be
  installed. Scheme is true for a color scheme of the source editor and false
  for a theme of the IDE. Replaces is true when the user has a theme or a
  scheme with the same name, which installing replaces. }

type
  TThemeDownload = record
    Name: string;
    Scheme: Boolean;
    Replaces: Boolean;
    Text: string;
  end;

{ Downloads a theme from a link, following redirects to the file. Returns
  false with the reason in Error if the link is not valid, the download
  fails, or the file is neither a theme nor a color scheme which can be
  installed. }
function ThemeDownload(const Link: string; out Download: TThemeDownload;
  out Error: string): Boolean;
{ Saves a downloaded theme with the themes or the color schemes of the user
  and adds it to the IDE, returning false with the reason in Error if it
  could not be added }
function ThemeInstall(const Download: TThemeDownload; out Error: string): Boolean;
{ True if the theme of the IDE named Name was installed by the user }
function ThemeInstalled(const Name: string): Boolean;
{ Removes a theme of the IDE installed by the user. When it is the theme in
  use, the default theme is applied. }
function ThemeRemove(const Name: string): Boolean;
{ True if the color scheme named Name was installed by the user }
function SchemeInstalled(const Name: string): Boolean;
{ Removes a color scheme installed by the user, along with the other
  schemes of its file }
function SchemeRemove(const Name: string): Boolean;

implementation

uses
  SysUtils, Classes, LazConfigStorage, BaseIDEIntf, LazIDEIntf,
  Laz2_DOM, Laz2_XMLRead,
  Codebot.Networking.Web,
  Codebot.Themes.Palettes,
  Codebot.Themes.Manager,
  Codebot.Themes.Editor;

const
  ConfigFile = 'codebotthemes.xml';
  ConfigPath = 'Theme/Name';
  { The folders of the themes and the color schemes of the user in the
    configuration folder of the IDE }
  UserFolder = 'codebotthemes';
  SchemeFolder = 'userschemes';
  MaxRedirects = 5;
  MaxSize = 512 * 1024;
  { The color schemes which come with the IDE }
  BuiltinSchemes: array[0..4] of string = ('Default', 'Twilight',
    'Pascal Classic', 'Ocean', 'Delphi');
  SystemName = 'System Default';

function ConfigFolder(const Name: string): string;
begin
  Result := '';
  if LazarusIDE <> nil then
    Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + Name;
end;

{ The package has no themes of its own, so every theme other than System
  Default is one the user installed }

procedure ThemesReload;
begin
  ThemesLoad([ConfigFolder(UserFolder)]);
end;

{ A file which is missing or can not be read leaves the default theme }

function ThemesLoadName: string;
var
  Storage: TConfigStorage;
begin
  Result := ThemeList[DefaultTheme].Name;
  if not Assigned(GetIDEConfigStorage) then
    Exit;
  try
    Storage := GetIDEConfigStorage(ConfigFile, True);
    try
      Result := Storage.GetValue(ConfigPath, Result);
    finally
      Storage.Free;
    end;
  except
    Result := ThemeList[DefaultTheme].Name;
  end;
end;

procedure ThemesSaveName;
var
  Storage: TConfigStorage;
begin
  if not Assigned(GetIDEConfigStorage) then
    Exit;
  Storage := GetIDEConfigStorage(ConfigFile, True);
  try
    Storage.SetValue(ConfigPath, ThemeList[ThemesCurrent].Name);
    Storage.WriteToDisk;
  finally
    Storage.Free;
  end;
end;

{ Reading and writing files }

function ReadText(const FileName: string): string;
var
  Stream: TStringStream;
begin
  Result := '';
  Stream := TStringStream.Create('');
  try
    try
      Stream.LoadFromFile(FileName);
      Result := Stream.DataString;
    except
      Result := '';
    end;
  finally
    Stream.Free;
  end;
end;

function WriteText(const FileName, Text: string): Boolean;
var
  Stream: TStringStream;
begin
  Result := ForceDirectories(ExtractFileDir(FileName));
  if not Result then
    Exit;
  Stream := TStringStream.Create(Text);
  try
    try
      Stream.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

{ The names of the color schemes of a scheme file }

function SchemeNames(const Text: string; Names: TStrings): Boolean;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  Node, Item: TDOMNode;
  S: string;
begin
  Names.Clear;
  Doc := nil;
  Stream := TStringStream.Create(Text);
  try
    try
      ReadXMLFile(Doc, Stream);
      Node := Doc.DocumentElement;
      if (Node = nil) or (Node.NodeName <> 'CONFIG') then
        Exit(False);
      Node := Node.FindNode('Lazarus');
      if Node <> nil then
        Node := Node.FindNode('ColorSchemes');
      if Node <> nil then
        Node := Node.FindNode('Names');
      if Node <> nil then
      begin
        Item := Node.FirstChild;
        while Item <> nil do
        begin
          if (Item is TDOMElement) and (Copy(Item.NodeName, 1, 4) = 'Item') then
          begin
            S := Trim(TDOMElement(Item).GetAttribute('Value'));
            if S <> '' then
              Names.Add(S);
          end;
          Item := Item.NextSibling;
        end;
      end;
    except
      Names.Clear;
    end;
  finally
    Doc.Free;
    Stream.Free;
  end;
  Result := Names.Count > 0;
end;

{ The file of the user which holds the theme or the color scheme named Name,
  or an empty string if the user has none }

function ThemeFile(const Name: string): string;
var
  Folder: string;
  Search: TSearchRec;
  Theme: TThemeInfo;
begin
  Result := '';
  Folder := ConfigFolder(UserFolder);
  if Folder = '' then
    Exit;
  Folder := IncludeTrailingPathDelimiter(Folder);
  if FindFirst(Folder + '*.json', faAnyFile, Search) = 0 then
  try
    repeat
      if ThemeLoadFile(Folder + Search.Name, Theme) and
        SameText(Theme.Name, Name) then
        Exit(Folder + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

function SchemeFile(const Name: string): string;
var
  Folder: string;
  Search: TSearchRec;
  Names: TStringList;
begin
  Result := '';
  Folder := ConfigFolder(SchemeFolder);
  if Folder = '' then
    Exit;
  Folder := IncludeTrailingPathDelimiter(Folder);
  Names := TStringList.Create;
  try
    if FindFirst(Folder + '*.xml', faAnyFile, Search) = 0 then
    try
      repeat
        if SchemeNames(ReadText(Folder + Search.Name), Names) and
          (Names.IndexOf(Name) >= 0) then
          Exit(Folder + Search.Name);
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
  finally
    Names.Free;
  end;
end;

{ A file name made from the name of a theme, which is not the name of a
  file already in the folder }

function NewFileName(const Folder, Name, Ext: string): string;
var
  Base: string;
  C: Char;
  I: Integer;
begin
  Base := '';
  for C in LowerCase(Name) do
    if C in ['a'..'z', '0'..'9'] then
      Base := Base + C
    else if (Base <> '') and (Base[Length(Base)] <> '-') then
      Base := Base + '-';
  while (Base <> '') and (Base[Length(Base)] = '-') do
    SetLength(Base, Length(Base) - 1);
  if Base = '' then
    Base := 'theme';
  Result := IncludeTrailingPathDelimiter(Folder) + Base + Ext;
  I := 1;
  while FileExists(Result) do
  begin
    Inc(I);
    Result := IncludeTrailingPathDelimiter(Folder) + Base + '-' + IntToStr(I) + Ext;
  end;
end;

function IsBuiltinScheme(const Name: string): Boolean;
var
  S: string;
begin
  for S in BuiltinSchemes do
    if SameText(S, Name) then
      Exit(True);
  Result := False;
end;

function SchemeExists(const Name: string): Boolean;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    EditorSchemeNames(Names);
    Result := Names.IndexOf(Name) >= 0;
  finally
    Names.Free;
  end;
end;

{ Downloading }

{ A redirect to a path is on the server of the link it came from }

function RedirectUrl(const Url: TUrl; const Location: string): TUrl;
var
  S: string;
begin
  if (Location = '') or (Location[1] <> '/') then
    Exit(TUrl.Create(Location));
  if Url.Secure then
    S := 'https://' + Url.Domain
  else
    S := 'http://' + Url.Domain;
  if (Url.Port <> 0) and (Url.Port <> 80) and (Url.Port <> 443) then
    S := S + ':' + IntToStr(Url.Port);
  Result := TUrl.Create(S + Location);
end;

{ A presigned link is used exactly as it is given, as changing it breaks its
  signature }

function DownloadText(const Link: string; out Text, Error: string): Boolean;
var
  Client: THttpClient;
  Url: TUrl;
  Location: string;
  I: Integer;
begin
  Result := False;
  Text := '';
  Url := TUrl.Create(Trim(Link));
  if not Url.Valid then
  begin
    Error := 'The link is not a valid url.';
    Exit;
  end;
  Client := THttpClient.Create;
  try
    for I := 0 to MaxRedirects do
    begin
      if not Client.Get(Url) then
      begin
        Error := 'The theme could not be downloaded from ' + Url.Domain + '.';
        Exit;
      end;
      case Client.Code of
        200:
          begin
            Text := Client.ResponseText;
            if Length(Text) > MaxSize then
            begin
              Error := 'The download is too large to be a theme.';
              Exit;
            end;
            Exit(True);
          end;
        301, 302, 303, 307, 308:
          begin
            Location := Trim(Client.Values['Location']);
            Url := RedirectUrl(Url, Location);
            if not Url.Valid then
            begin
              Error := 'The link was redirected to a url which is not valid.';
              Exit;
            end;
          end;
        403, 404:
          begin
            Error := 'The theme was not found, or its link has expired.';
            Exit;
          end;
      else
        Error := Format('The server answered %d %s.', [Client.Code, Client.Status]);
        Exit;
      end;
    end;
    Error := 'The link was redirected too many times.';
  finally
    Client.Free;
  end;
end;

function ThemeDownload(const Link: string; out Download: TThemeDownload;
  out Error: string): Boolean;
var
  Theme: TThemeInfo;
  Names: TStringList;
  S: string;
begin
  Result := False;
  Error := '';
  Download.Name := '';
  Download.Scheme := False;
  Download.Replaces := False;
  Download.Text := '';
  if not DownloadText(Link, S, Error) then
    Exit;
  S := Trim(S);
  { A byte order mark is left out }
  if Copy(S, 1, 3) = #$EF#$BB#$BF then
    S := Trim(Copy(S, 4, Length(S)));
  Download.Text := S;
  if (S <> '') and (S[1] = '{') then
  begin
    if not ThemeLoadText(S, Theme) then
    begin
      Error := 'The download is not a theme which can be read.';
      Exit;
    end;
    if SameText(Theme.Name, SystemName) then
    begin
      Error := 'The theme has the name of a theme which can not be replaced.';
      Exit;
    end;
    Download.Name := Theme.Name;
    Download.Replaces := ThemeFile(Theme.Name) <> '';
    Exit(True);
  end;
  if (S <> '') and (S[1] = '<') then
  begin
    Names := TStringList.Create;
    try
      if not SchemeNames(S, Names) then
      begin
        Error := 'The download is not a color scheme which can be read.';
        Exit;
      end;
      Download.Name := Names[0];
    finally
      Names.Free;
    end;
    if IsBuiltinScheme(Download.Name) then
    begin
      Error := 'The color scheme has the name of a scheme which comes with Lazarus.';
      Exit;
    end;
    if SchemeExists(Download.Name) and (SchemeFile(Download.Name) = '') then
    begin
      Error := 'Lazarus already has a color scheme named ' + Download.Name + '.';
      Exit;
    end;
    Download.Scheme := True;
    Download.Replaces := SchemeExists(Download.Name);
    Exit(True);
  end;
  Error := 'The download is neither a theme nor a color scheme.';
end;

{ Installing and removing }

function ThemeInstalled(const Name: string): Boolean;
begin
  Result := ThemeFile(Name) <> '';
end;

function SchemeInstalled(const Name: string): Boolean;
begin
  Result := (not IsBuiltinScheme(Name)) and (SchemeFile(Name) <> '');
end;

{ The themes are loaded again after a file is added or removed. The theme in
  use is applied again, as its colors may have changed, or the default theme
  when it is gone. }

procedure ThemesChanged(const InUse: string);
var
  I: Integer;
begin
  ThemesReload;
  I := IndexOfTheme(InUse);
  if I < 0 then
    I := DefaultTheme;
  ThemesApply(I);
  ThemesSaveName;
end;

function ThemeInstall(const Download: TThemeDownload; out Error: string): Boolean;
var
  FileName, Name, InUse: string;
begin
  Result := False;
  Error := '';
  if Download.Scheme then
  begin
    if Download.Replaces then
      SchemeRemove(Download.Name);
    FileName := NewFileName(ConfigFolder(SchemeFolder), Download.Name, '.xml');
    if not WriteText(FileName, Download.Text) then
    begin
      Error := 'The color scheme could not be saved.';
      Exit;
    end;
    if not EditorSchemeAdd(FileName, Name) then
    begin
      DeleteFile(FileName);
      Error := 'Lazarus could not read the color scheme.';
      Exit;
    end;
    Exit(True);
  end;
  InUse := ThemeList[ThemesCurrent].Name;
  if Download.Replaces then
    DeleteFile(ThemeFile(Download.Name));
  FileName := NewFileName(ConfigFolder(UserFolder), Download.Name, '.json');
  if not WriteText(FileName, Download.Text) then
  begin
    Error := 'The theme could not be saved.';
    Exit;
  end;
  ThemesChanged(InUse);
  Result := True;
end;

function ThemeRemove(const Name: string): Boolean;
var
  FileName, InUse: string;
begin
  FileName := ThemeFile(Name);
  Result := (FileName <> '') and DeleteFile(FileName);
  if not Result then
    Exit;
  InUse := ThemeList[ThemesCurrent].Name;
  ThemesChanged(InUse);
end;

function SchemeRemove(const Name: string): Boolean;
var
  FileName, S: string;
  Names: TStringList;
begin
  Result := False;
  if IsBuiltinScheme(Name) then
    Exit;
  FileName := SchemeFile(Name);
  if FileName = '' then
    Exit;
  Names := TStringList.Create;
  try
    SchemeNames(ReadText(FileName), Names);
    for S in Names do
      EditorSchemeRemove(S);
  finally
    Names.Free;
  end;
  Result := DeleteFile(FileName);
end;

end.

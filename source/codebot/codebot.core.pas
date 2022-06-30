(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.core.txt> }
unit Codebot.Core;

{$i codebot.inc}

interface

uses
  {$ifdef unix}
  CThreads,
  {$endif}
  DynLibs;

type
  HModule = TLibHandle;

const
  ModuleNil = HModule(0);
  SharedSuffix = DynLibs.SharedSuffix;

function LibraryLoad(const Name: string): HModule; overload;
function LibraryLoad(const Name, AltName: string): HModule; overload;
function LibraryUnload(Module: HModule): Boolean;
function LibraryGetProc(Module: HModule; const ProcName: string): Pointer;

var
  LibraryExceptProc: procedure(const ModuleName: string; ProcName: string);

implementation

function LibraryLoad(const Name: string): HModule;
begin
  Result := LoadLibrary(Name);
end;

function LibraryLoad(const Name, AltName: string): HModule;
begin
  Result := LoadLibrary(Name);
  if Result = ModuleNil then
    Result := LoadLibrary(AltName);
end;

function LibraryUnload(Module: HModule): Boolean;
begin
  if Module <> ModuleNil then
    Result := UnloadLibrary(Module)
  else
    Result := False;
end;

function LibraryGetProc(Module: HModule; const ProcName: string): Pointer;
begin
  if Module <> ModuleNil then
    Result := GetProcAddress(Module, ProcName)
  else
    Result := nil;
end;

end.


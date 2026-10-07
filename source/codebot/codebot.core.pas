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
  { HModule is a handle to a loaded dynamic library }
  HModule = TLibHandle;

const
  { ModuleNil is the value of an HModule which refers to no library }
  ModuleNil = HModule(0);
  { SharedSuffix is the file extension of a dynamic library on the current
    operating system }
  SharedSuffix = DynLibs.SharedSuffix;

{ Load a dynamic library by name returning ModuleNil if it could not be loaded }
function LibraryLoad(const Name: string): HModule; overload;
{ Load a dynamic library by name, trying AltName if Name could not be loaded }
function LibraryLoad(const Name, AltName: string): HModule; overload;
{ Unload a dynamic library returning true if it was unloaded }
function LibraryUnload(Module: HModule): Boolean;
{ Return the address of an exported function in a dynamic library or nil if
  the function could not be found }
function LibraryGetProc(Module: HModule; const ProcName: string): Pointer;

{ LibraryExceptProc is invoked by interop units when a library or function
  fails to load. Codebot.System assigns it to raise an ELibraryException. }
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


(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2023                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.openssl.txt> }
unit Codebot.Interop.OpenSSL;

{$i codebot.inc}

interface

uses
  Codebot.Core;

const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;

  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
  TLSEXT_NAMETYPE_host_name = 0;

  SSL_FILETYPE_ASN1	= 2;
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;

  EVP_MAX_MD_SIZE = 64;
  EVP_MAX_KEY_LENGTH = 64;
  EVP_MAX_IV_LENGTH = 16;
  EVP_MAX_BLOCK_LENGTH = 32;

type
  TSSLCtxPrivate = record end;
  TSSLCtx = ^TSSLCtxPrivate;

  TSSLPrivate = record end;
  TSSL = ^TSSLPrivate;

  TSSLMethodPrivate = record end;
  TSSLMethod = ^TSSLMethodPrivate;

  TEVPMethodPrivate = record end;
  TEVPMethod = ^TEVPMethodPrivate;

  TEVPMdCtxPrivate = record end;
  TEVPMdCtx = ^TEVPMdCtxPrivate;

  THMACCtxPrivate = record end;
  THMACCtx = ^THMACCtxPrivate;

  X509 = Pointer;
  TX509 = X509;

  EVP_PKEY = Pointer;
  TEVPPKey = EVP_PKEY;

  RSA = Pointer;
  TRSA = RSA;

{ OpenSSL routines }

var
  TLS_method: function: TSSLMethod; cdecl;
  TLS_client_method: function: TSSLMethod; cdecl;
  TLS_server_method: function: TSSLMethod; cdecl;
  SSL_CTX_new: function(method: TSSLMethod): TSSLCtx; cdecl;
  SSL_CTX_free: procedure(context: TSSLCtx); cdecl;
  SSL_new: function(context: TSSLCtx): TSSL; cdecl;
  SSL_shutdown: function(ssl: TSSL): LongInt; cdecl;
  SSL_free: procedure(ssl: TSSL); cdecl;
  SSL_ctrl: function(ssl: TSSL; cmd: Integer; arg: LongInt; param: Pointer): LongInt; cdecl;
  SSL_set_fd: function(ssl: TSSL; socket: LongInt): LongInt; cdecl;
  SSL_accept: function(ssl: TSSL): LongInt; cdecl;
  SSL_connect: function(ssl: TSSL): LongInt; cdecl;
  SSL_write: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_read: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_get_error: function(ssl: TSSL; ret_code: Integer): Integer; cdecl;
  SSL_CTX_use_certificate: function(context: TSSLCtx; x: TX509): LongInt; cdecl;
  SSL_CTX_use_certificate_ASN1: function(context: TSSLCtx; len: LongInt; data: PChar): LongInt; cdecl;
  SSL_CTX_use_certificate_file: function(context: TSSLCtx; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_use_certificate: function(ssl: TSSL; x: TX509): LongInt; cdecl;
  SSL_use_certificate_ASN1: function(ssl: TSSL; data: PChar; len: LongInt): LongInt; cdecl;
  SSL_use_certificate_file: function(ssl: TSSL; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_CTX_use_certificate_chain_file: function(context: TSSLCtx; filename: PChar): LongInt; cdecl;
  SSL_use_certificate_chain_file: function(ssl: TSSL; filename: PChar): LongInt; cdecl;
  SSL_CTX_use_PrivateKey: function(context: TSSLCtx; key: TEVPPKey): LongInt; cdecl;
  SSL_CTX_use_PrivateKey_ASN1: function(pk: LongInt; context: TSSLCtx; data: PChar; len: NativeInt): LongInt; cdecl;
  SSL_CTX_use_PrivateKey_file: function(context: TSSLCtx; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_CTX_use_RSAPrivateKey: function(context: TSSLCtx; rsa: TRSA): LongInt; cdecl;
  SSL_CTX_use_RSAPrivateKey_ASN1: function(context: TSSLCtx; data: PChar; len: NativeInt): LongInt; cdecl;
  SSL_CTX_use_RSAPrivateKey_file: function(context: TSSLCtx; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_use_PrivateKey: function(ssl: TSSL; pkey: TEVPPKey): LongInt; cdecl;
  SSL_use_PrivateKey_ASN1: function(pk: LongInt; ssl: TSSL; data: PChar; len: NativeInt): LongInt; cdecl;
  SSL_use_PrivateKey_file: function(ssl: TSSL; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_use_RSAPrivateKey: function(ssl: TSSL; rsa: TRSA): LongInt; cdecl;
  SSL_use_RSAPrivateKey_ASN1: function(ssl: TSSL; data: PChar; len: NativeInt): LongInt; cdecl;
  SSL_use_RSAPrivateKey_file: function(ssl: TSSL; filename: PChar; kind: LongInt): LongInt; cdecl;
  SSL_CTX_check_private_key: function(context: TSSLCtx): LongInt; cdecl;
  SSL_check_private_key: function(ssl: TSSL): LongInt; cdecl;

{ Hashing routines }

  EVP_md5: function: TEVPMethod; cdecl;
  EVP_sha1: function: TEVPMethod; cdecl;
  EVP_sha256: function: TEVPMethod; cdecl;
  EVP_sha512: function: TEVPMethod; cdecl;

  EVP_MD_CTX_new: function: TEVPMdCtx; cdecl;
  EVP_MD_CTX_reset: function(ctx: TEVPMdCtx): Integer; cdecl;
  EVP_MD_CTX_free: procedure(ctx: TEVPMdCtx); cdecl;

  EVP_DigestInit_ex: function(ctx: TEVPMdCtx; method: TEVPMethod; engine: Pointer = nil): LongBool; cdecl;
  EVP_DigestUpdate: function(ctx: TEVPMdCtx; data: Pointer; dataLen: Cardinal): LongBool; cdecl;
  EVP_DigestFinal: function(ctx: TEVPMdCtx; digest: Pointer; out digestLen: Cardinal): LongBool; cdecl;

  HMAC_CTX_new: function: THMACCtx; cdecl;
  HMAC_CTX_reset: function(ctx: THMACCtx): Integer; cdecl;
  HMAC_CTX_free: procedure(ctx: THMACCtx); cdecl;

  HMAC_Init_ex: function(ctx: THMACCtx; key: Pointer; keyLen: Cardinal; method: TEVPMethod;
    engine: Pointer = nil): LongBool; cdecl;
  HMAC_Update: function(ctx: THMACCtx; data: Pointer; dataLen: Cardinal): LongBool; cdecl;
  HMAC_Final: function(ctx: THMACCtx; digest: Pointer; out digestLen: Cardinal): LongBool; cdecl;

  HMAC: function(method: TEVPMethod; key: Pointer; keyLen: Cardinal; data: Pointer; dataLen: Cardinal;
    digest: Pointer; out digestLen: Cardinal): Pointer; cdecl;

const
{$ifdef windows}
  libssl = 'libssl32.dll';
  libcrypto = 'libeay32.dll';
{$endif}
{$ifdef linux}
  libssl = 'libssl.so.3';
  libcrypto = 'libcrypto.so.3';
{$endif}

function InitSSL(ThrowExceptions: Boolean = False): Boolean;
function InitCrypto(ThrowExceptions: Boolean = False): Boolean;

implementation

var
  LoadedSSL: Boolean;
  InitializedSSL: Boolean;
  LoadedCrypto: Boolean;
  InitializedCrypto: Boolean;

function InitSSL(ThrowExceptions: Boolean = False): Boolean;
var
  FailedModuleName: string;
  FailedProcName: string;
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not InitializedSSL) and (ThrowExceptions) then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
    begin
      CheckExceptions;
    end;
  end;

begin
  ThrowExceptions := ThrowExceptions and (@LibraryGetProc <> nil);
  if LoadedSSL then
  begin
    CheckExceptions;
    Exit(InitializedSSL);
  end;
  LoadedSSL:= True;
  if InitializedSSL then
    Exit(True);
  Result := False;
  FailedModuleName := libssl;
  FailedProcName := '';
  Module := LibraryLoad(libssl);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('TLS_method', @TLS_method) and
    TryLoad('TLS_client_method', @TLS_client_method) and
    TryLoad('TLS_server_method', @TLS_server_method) and
    TryLoad('SSL_CTX_new', @SSL_CTX_new) and
    TryLoad('SSL_CTX_free', @SSL_CTX_free) and
    TryLoad('SSL_new', @SSL_new) and
    TryLoad('SSL_shutdown', @SSL_shutdown) and
    TryLoad('SSL_free', @SSL_free) and
    TryLoad('SSL_ctrl', @SSL_ctrl) and
    TryLoad('SSL_set_fd', @SSL_set_fd) and
    TryLoad('SSL_accept', @SSL_accept) and
    TryLoad('SSL_connect', @SSL_connect) and
    TryLoad('SSL_write', @SSL_write) and
    TryLoad('SSL_read', @SSL_read) and
    TryLoad('SSL_get_error', @SSL_get_error) and
    TryLoad('SSL_CTX_use_certificate', @SSL_CTX_use_certificate) and
    TryLoad('SSL_CTX_use_certificate_ASN1', @SSL_CTX_use_certificate_ASN1) and
    TryLoad('SSL_CTX_use_certificate_file', @SSL_CTX_use_certificate_file) and
    TryLoad('SSL_use_certificate', @SSL_use_certificate) and
    TryLoad('SSL_use_certificate_ASN1', @SSL_use_certificate_ASN1) and
    TryLoad('SSL_use_certificate_file', @SSL_use_certificate_file) and
    TryLoad('SSL_CTX_use_certificate_chain_file', @SSL_CTX_use_certificate_chain_file) and
    TryLoad('SSL_CTX_use_PrivateKey', @SSL_CTX_use_PrivateKey) and
    TryLoad('SSL_CTX_use_PrivateKey_ASN1', @SSL_CTX_use_PrivateKey_ASN1) and
    TryLoad('SSL_CTX_use_PrivateKey_file', @SSL_CTX_use_PrivateKey_file) and
    TryLoad('SSL_CTX_use_RSAPrivateKey', @SSL_CTX_use_RSAPrivateKey) and
    TryLoad('SSL_CTX_use_RSAPrivateKey_ASN1', @SSL_CTX_use_RSAPrivateKey_ASN1) and
    TryLoad('SSL_CTX_use_RSAPrivateKey_file', @SSL_CTX_use_RSAPrivateKey_file) and
    TryLoad('SSL_use_PrivateKey', @SSL_use_PrivateKey) and
    TryLoad('SSL_use_PrivateKey_ASN1', @SSL_use_PrivateKey_ASN1) and
    TryLoad('SSL_use_PrivateKey_file', @SSL_use_PrivateKey_file) and
    TryLoad('SSL_use_RSAPrivateKey', @SSL_use_RSAPrivateKey) and
    TryLoad('SSL_use_RSAPrivateKey_ASN1', @SSL_use_RSAPrivateKey_ASN1) and
    TryLoad('SSL_use_RSAPrivateKey_file', @SSL_use_RSAPrivateKey_file) and
    TryLoad('SSL_CTX_check_private_key', @SSL_CTX_check_private_key) and
    TryLoad('SSL_check_private_key', @SSL_check_private_key);
  InitializedSSL := Result;
end;

function InitCrypto(ThrowExceptions: Boolean = False): Boolean;
var
  FailedModuleName: string;
  FailedProcName: string;
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not InitializedCrypto) and (ThrowExceptions) then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
    begin
      CheckExceptions;
    end;
  end;

begin
  ThrowExceptions := ThrowExceptions and (@LibraryGetProc <> nil);
  if LoadedCrypto then
  begin
    CheckExceptions;
    Exit(InitializedCrypto);
  end;
  LoadedCrypto:= True;
  if InitializedCrypto then
    Exit(True);
  Result := False;
  FailedModuleName := libcrypto;
  FailedProcName := '';
  Module := LibraryLoad(FailedModuleName);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('EVP_md5', @EVP_md5) and
    TryLoad('EVP_sha1', @EVP_sha1) and
    TryLoad('EVP_sha256', @EVP_sha256) and
    TryLoad('EVP_sha512', @EVP_sha512) and
    TryLoad('EVP_MD_CTX_new', @EVP_MD_CTX_new) and
    TryLoad('EVP_MD_CTX_reset', @EVP_MD_CTX_reset) and
    TryLoad('EVP_MD_CTX_free', @EVP_MD_CTX_free) and
    TryLoad('EVP_DigestInit_ex', @EVP_DigestInit_ex) and
    TryLoad('EVP_DigestUpdate', @EVP_DigestUpdate) and
    TryLoad('EVP_DigestFinal', @EVP_DigestFinal) and
    TryLoad('HMAC_CTX_new', @HMAC_CTX_new) and
    TryLoad('HMAC_CTX_reset', @HMAC_CTX_reset) and
    TryLoad('HMAC_CTX_free', @HMAC_CTX_free) and
    TryLoad('HMAC_Init_ex', @HMAC_Init_ex) and
    TryLoad('HMAC_Update', @HMAC_Update) and
    TryLoad('HMAC_Final', @HMAC_Final) and
    TryLoad('HMAC', @HMAC);
  InitializedCrypto := Result;
end;

end.

(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
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

  MD5_DIGEST_LENGTH = 16;
  SHA1_DIGEST_LENGTH = 20;
  SHA256_DIGEST_LENGTH = 32;
  SHA512_DIGEST_LENGTH = 64;

type
  TSSLCtx = Pointer;
  TSSL = Pointer;
  TSSLMethod = Pointer;
  TEVPMethod = Pointer;

  MD5_CTX = record
    data: array[0..127] of Byte;
  end;
  TMD5Ctx = MD5_CTX;
  PMD5Ctx = ^TMD5Ctx;

  MD5_DIGEST = record
    data: array [0..MD5_DIGEST_LENGTH - 1] of Byte;
  end;
  TMD5Digest = MD5_DIGEST;
  PMD5Digest = ^TMD5Digest;

  SHA1_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA1Ctx = SHA1_CTX;
  PSHA1Ctx = ^TSHA1Ctx;

  SHA1_DIGEST = record
    data: array [0..SHA1_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA1Digest = SHA1_DIGEST;
  PSHA1Digest = ^TSHA1Digest;

  SHA256_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA256Ctx = SHA256_CTX;
  PSHA256Ctx = ^TSHA256Ctx;

  SHA256_DIGEST = record
    data: array [0..SHA256_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA256Digest = SHA256_DIGEST;
  PSHA256Digest = ^TSHA256Digest;

  SHA512_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA512Ctx = SHA512_CTX;
  PSHA512Ctx = ^TSHA512Ctx;

  SHA512_DIGEST = record
    data: array [0..SHA512_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA512Digest = SHA512_DIGEST;
  PSHA512Digest = ^TSHA512Digest;

  HMAC_CTX = record
    data: array[0..511] of Byte;
  end;
  THMACCtx = HMAC_CTX;
  PHMACCtx = ^THMACCtx;

{ OpenSSL routines }

var
  SSL_library_init: function: Integer; cdecl;
  SSL_load_error_strings: procedure; cdecl;
  SSLv23_client_method: function: TSSLMethod; cdecl;
  SSL_CTX_new: function(method: TSSLMethod): TSSLCtx; cdecl;
  SSL_CTX_free: procedure(context: TSSLCtx); cdecl;
  SSL_new: function(context: TSSLCtx): TSSL; cdecl;
  SSL_shutdown: function(ssl: TSSL): LongInt; cdecl;
  SSL_free: procedure(ssl: TSSL); cdecl;
  SSL_set_fd: function(ssl: TSSL; socket: LongInt): LongBool; cdecl;
  SSL_connect: function(ssl: TSSL): LongBool; cdecl;
  SSL_write: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_read: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_get_error: function(ssl: TSSL; ret_code: Integer): Integer; cdecl;

{ Hashing routines }

  MD5_Init: function(out context: TMD5Ctx): LongBool; cdecl;
  MD5_Update: function(var context: TMD5Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  MD5_Final: function(out digest: TMD5Digest; var context: TMD5Ctx): LongBool; cdecl;
  SHA1_Init: function(out context: TSHA1Ctx): LongBool; cdecl;
  SHA1_Update: function(var context: TSHA1Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA1_Final: function(out digest: TSHA1Digest; var context: TSHA1Ctx): LongBool; cdecl;
  SHA256_Init: function(out context: TSHA256Ctx): LongBool; cdecl;
  SHA256_Update: function(var context: TSHA256Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA256_Final: function(out digest: TSHA256Digest; var context: TSHA256Ctx): LongBool; cdecl;
  SHA512_Init: function(out context: TSHA512Ctx): LongBool; cdecl;
  SHA512_Update: function(var context: TSHA512Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA512_Final: function(out digest: TSHA512Digest; var context: TSHA512Ctx): LongBool; cdecl;
  EVP_md5: function: TEVPMethod; cdecl;
  EVP_sha1: function: TEVPMethod; cdecl;
  EVP_sha256: function: TEVPMethod; cdecl;
  EVP_sha512: function: TEVPMethod; cdecl;
  HMAC_CTX_init: procedure(out context: THMACCtx); cdecl;
  HMAC_CTX_cleanup: procedure(var context: THMACCtx); cdecl;
  HMAC_Init_ex: function(var context: THMACCtx; key: Pointer; size: Cardinal; method: TEVPMethod; engine: Pointer): LongBool; cdecl;
  HMAC_Update: function(var context: THMACCtx; data: Pointer; size: Cardinal): LongBool; cdecl;
  HMAC_Final: function(var context: THMACCtx; digest: Pointer; var digestSize: LongWord): LongBool; cdecl;

const
{$ifdef windows}
  libssl = 'libssl32.dll';
  libssl100 = libssl;
  libcrypto = 'libeay32.dll';
  libcrypto1 = libcrypto;
{$endif}
{$ifdef linux}
  libssl = 'libssl.' + SharedSuffix;
  libssl100 = libssl + '.1.0.0';
  libcrypto = 'libcrypto.' + SharedSuffix;
  libcrypto1 = libcrypto + '.1';
{$endif}

function OpenSSLInit(ThrowExceptions: Boolean = False): Boolean;

implementation

var
  Loaded: Boolean;
  Initialized: Boolean;
  FailedModuleName: string;
  FailedProcName: string;

function OpenSSLInit(ThrowExceptions: Boolean = False): Boolean;
var
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not Initialized) and (ThrowExceptions) then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
      CheckExceptions;
  end;

begin
  ThrowExceptions := ThrowExceptions and (@LibraryGetProc <> nil);
  if Loaded then
  begin
    CheckExceptions;
    Exit(Initialized);
  end;
  Loaded:= True;
  if Initialized then
    Exit(True);
  Result := False;
  FailedModuleName := libssl;
  FailedProcName := '';
  Module := LibraryLoad(libssl, libssl100);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('SSL_library_init', @SSL_library_init) and
    TryLoad('SSL_library_init', @SSL_library_init) and
    TryLoad('SSL_load_error_strings', @SSL_load_error_strings) and
    TryLoad('SSLv23_client_method', @SSLv23_client_method) and
    TryLoad('SSL_CTX_new', @SSL_CTX_new) and
    TryLoad('SSL_CTX_free', @SSL_CTX_free) and
    TryLoad('SSL_new', @SSL_new) and
    TryLoad('SSL_shutdown', @SSL_shutdown) and
    TryLoad('SSL_free', @SSL_free) and
    TryLoad('SSL_set_fd', @SSL_set_fd) and
    TryLoad('SSL_connect', @SSL_connect) and
    TryLoad('SSL_write', @SSL_write) and
    TryLoad('SSL_read', @SSL_read) and
    TryLoad('SSL_get_error', @SSL_get_error);
  if not Result then
    Exit;
  Result := False;
  FailedModuleName := libcrypto;
  FailedProcName := '';
  Module := LibraryLoad(libcrypto, libcrypto1);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('MD5_Init', @MD5_Init) and
    TryLoad('MD5_Update', @MD5_Update) and
    TryLoad('MD5_Final', @MD5_Final) and
    TryLoad('SHA1_Init', @SHA1_Init) and
    TryLoad('SHA1_Update', @SHA1_Update) and
    TryLoad('SHA1_Final', @SHA1_Final) and
    TryLoad('SHA256_Init', @SHA256_Init) and
    TryLoad('SHA256_Update', @SHA256_Update) and
    TryLoad('SHA256_Final', @SHA256_Final) and
    TryLoad('SHA512_Init', @SHA512_Init) and
    TryLoad('SHA512_Update', @SHA512_Update) and
    TryLoad('SHA512_Final', @SHA512_Final) and
    TryLoad('EVP_md5', @EVP_md5) and
    TryLoad('EVP_sha1', @EVP_sha1) and
    TryLoad('EVP_sha256', @EVP_sha256) and
    TryLoad('EVP_sha512', @EVP_sha512) and
    TryLoad('HMAC_CTX_init', @HMAC_CTX_init) and
    TryLoad('HMAC_CTX_cleanup', @HMAC_CTX_cleanup) and
    TryLoad('HMAC_Init_ex', @HMAC_Init_ex) and
    TryLoad('HMAC_Update', @HMAC_Update) and
    TryLoad('HMAC_Final', @HMAC_Final);
  if not Result then
    Exit;
  FailedModuleName := '';
  FailedProcName := '';;
  Initialized := True;
  SSL_library_init;
  SSL_load_error_strings;
end;

end.

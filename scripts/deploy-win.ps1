# Builds trimmed copies of fpc and lazarus for 64 bit Windows in the deploy
# folder, packs them as zip archives and publishes them as a GitHub release.
# Windows releases are tagged build-win-N, so they never clash with the
# build-N releases made by deploy-linux.sh.
#
# Use -Force to make and publish a new build even when the archives exist,
# such as after updating only the DLLs.

param([switch]$Force)

$ErrorActionPreference = 'Stop'
Set-Location $PSScriptRoot

$Target = 'x86_64-win64'
$Repo = 'sysrpl/Codebot.Cross'
$Utf8 = New-Object System.Text.UTF8Encoding $false

# Everything this script deletes must be inside the deploy folder
$DeployDir = Join-Path $PSScriptRoot deploy

# Deletes a file or folder, refusing anything outside the deploy folder. Paths
# are joined to the current folder first, so a name such as ~ is never read as
# the home folder.
function Remove-Safe([string]$Path) {
  $Full = [IO.Path]::GetFullPath($Path)
  if (-not $Full.StartsWith("$DeployDir\", [StringComparison]::OrdinalIgnoreCase)) {
    throw "Refusing to delete $Full, it is outside $DeployDir"
  }
  if (Test-Path -LiteralPath $Full) { Remove-Item -LiteralPath $Full -Recurse -Force }
}

# Deletes files or folders relative to the current folder, ignoring those
# which do not exist
function Remove-Paths([string[]]$Paths) {
  foreach ($P in $Paths) { Remove-Safe (Join-Path (Get-Location).Path $P) }
}

# Deletes found items, shortest path first, skipping those already removed
# with a parent folder
function Remove-Found($Items) {
  $Items | Sort-Object { $_.FullName.Length } | ForEach-Object { Remove-Safe $_.FullName }
}

function Read-Text($Name) { [IO.File]::ReadAllText((Resolve-Path $Name)) }
function Write-Text($Name, $Text) { [IO.File]::WriteAllText((Resolve-Path $Name), $Text, $Utf8) }

# Read the versions from the original source trees, the deploy copy of fpc has
# no compiler folder
$V = Get-Content fpc\compiler\version.pas -Raw
$FpcVersion = ('version_nr', 'release_nr', 'patch_nr' |
  ForEach-Object { [regex]::Match($V, "$_\s*=\s*'(\d+)'").Groups[1].Value }) -join '.'
$V = Get-Content lazarus\components\lazutils\lazversion.pas -Raw
$LazVersion = ('laz_major', 'laz_minor' |
  ForEach-Object { [regex]::Match($V, "$_\s*=\s*(\d+);").Groups[1].Value }) -join '.'

New-Item -ItemType Directory -Force deploy | Out-Null

foreach ($Dir in 'fpc', 'lazarus') {
  if (-not (Test-Path "deploy\$Dir")) {
    robocopy $Dir "deploy\$Dir" /E /NFL /NDL /NJH /NJS /NP | Out-Null
    if ($LASTEXITCODE -ge 8) { throw "Could not copy $Dir" }
  }
}

# Claude Code session files, such as .claude-history transcripts, are never
# published
Remove-Found (Get-ChildItem deploy\fpc, deploy\lazarus -Recurse -Force -Filter '.claude*')

# Trim fpc
Push-Location deploy\fpc
try {
  Remove-Paths .git, tests, utils, compiler, share, installer
  Remove-Paths "bin\$Target\fpc.cfg", "bin\$Target\fp.exe"

  # Linux files: the binaries directly in bin, the Linux layout in lib and the
  # Linux setup script. Windows uses bin\x86_64-win64 and units\x86_64-win64.
  Get-ChildItem bin -File | Remove-Item -Force
  Remove-Paths "lib\fpc\$FpcVersion", lib\libpas2jslib.so, setup

  # Build output in the source trees (units holds the real units)
  Remove-Found (Get-ChildItem rtl, packages -Directory -Recurse -Force -Filter units)
  Get-ChildItem rtl, packages -File -Recurse -Force -Include *.o, *.ppu, *.a, *.rsj, *.compiled, *.fpm |
    Remove-Item -Force

  # Git files
  Remove-Found (Get-ChildItem -Recurse -Force -Filter '.git*')

  # Make and fpmake support files
  Remove-Paths packages\fpmake
  Get-ChildItem -Recurse -File -Force -Include Makefile, Makefile.*, fpmake.pp, fpmake*.inc, Package.fpc |
    Where-Object { $_.FullName -notmatch '\\(units|lib)\\' } |
    Remove-Item -Force

  # Unneeded packages, both compiled units and sources. No package kept depends
  # on these. Kept because they are needed: libtar (fpmkunit, used by the IDE),
  # tplylib (fcl-res), fastcgi, httpd22, httpd24, libmicrohttpd (fcl-web, fppkg),
  # pastojs, webidl (utils-pas2js), numlib (TAChart), libcups (Printer4Lazarus)
  foreach ($P in 'googleapi odata fcl-report fcl-sdo ide
    aspell bfd cdrom dts fftw fuse fv gdbint gmp gnutls graph hermes
    httpd13 httpd20 imagemagick imlib jni ldap libc libenet
    libgc libgd libmagic libsee libusb libvlc lua mad matroska
    modplug newt oggvorbis opencl pcap proj4 ptc sdl symbolic tcl
    users utmp xforms zorba gtk1 fpgtk gnome1 ggi svgalib' -split '\s+') {
    Remove-Paths "packages\$P", "units\$Target\$P", "fpmkinst\$Target\$P.fpm"
  }

  # Source only, the compiled units stay in units
  Remove-Paths packages\rtl-unicode

  # Package examples
  Remove-Found (Get-ChildItem packages -Directory -Recurse -Force -Filter examples)

  # Debug info in the compiled units, stripped in batches to keep the command
  # line short
  $Strip = (Resolve-Path "bin\$Target\strip.exe").Path
  $Objects = @(Get-ChildItem units -Recurse -File -Filter *.o | ForEach-Object FullName)
  for ($I = 0; $I -lt $Objects.Count; $I += 100) {
    & $Strip --strip-debug $Objects[$I..([Math]::Min($I + 99, $Objects.Count - 1))]
    if ($LASTEXITCODE -ne 0) { throw 'Could not strip the compiled units' }
  }

  # Other platforms
  Push-Location rtl
  Remove-Paths ('aarch64 aix amicommon amiga android arm aros atari avr beos bsd darwin
    dragonfly embedded emx freebsd gba go32v2 haiku i386 i8086 java jvm linux m68k macos
    mips mipsel morphos msdos nativent nds netbsd netware netwlibc openbsd os2 palmos
    powerpc powerpc64 qnx solaris sparc sparc64 symbian unix watcom wii win16 win32
    wince' -split '\s+')
  Pop-Location
  Push-Location packages
  Remove-Paths ('ami-extra amunits arosunits cocoaint iosxlocale libgbafpc libndsfpc
    libogcfpc morphunits nvapi objcrtl os2units os4units palmunits rexx tosunits
    univint winceunits' -split '\s+')
  Pop-Location
}
finally { Pop-Location }

# Trim lazarus
Push-Location deploy\lazarus
try {
  Remove-Paths examples, lazarus.old, lazarus.old.exe, test
  Get-ChildItem -File -Filter 'link*.res' | Remove-Item -Force

  # Linux files and a stray folder
  Remove-Paths lazbuild, startlazarus, lazarus.sh, '~'

  # macOS app bundles
  Remove-Found (Get-ChildItem -Directory -Recurse -Force -Filter '*.app')

  # Compiled output folders, lazbuild compiles the units again when needed.
  # The IDE in lazarus.exe is kept with the codebot packages built in.
  Remove-Found (Get-ChildItem -Directory -Recurse -Force |
    Where-Object { $_.Name -eq $Target -or $_.Name -eq 'x86_64-linux' })

  # Docs, keep the xml used for IDE hints, the help files the IDE reads and licenses
  Push-Location docs
  Remove-Paths ('chm diagrams html images index.html LazarusIDEInternals.pdf
    acknowledgements.txt BigIDE.txt Contributors.txt CrossCompile.txt
    DesignGuidelines.txt ExtendingTheIDE.txt ForDelphians.txt INSTALL.txt
    LCLMessages.txt RemoteDebugging.txt SVN.txt' -split '\s+')
  Pop-Location

  Push-Location config
  try {
    # Files Lazarus writes again by itself, which hold paths and caches from
    # other computers and compiler versions: the IDE build options, the code
    # tools cache of compiler defines and unit paths, and the include file links
    Remove-Paths idemake.cfg, fpcdefines.xml, includelinks.xml

    # The default options for new projects saved from another project
    Remove-Paths projectoptions.xml

    # Input history, project sessions and backups
    Remove-Paths inputhistory.xml, projectsessions, backup

    # Settings pointing at files on other computers, Lazarus uses its defaults
    # without them
    Write-Text codetoolsoptions.xml ((Read-Text codetoolsoptions.xml) -replace '[ \t]*<Indentation FileName=[^>]*/>\r?\n', '')
    Write-Text editoroptions.xml ((Read-Text editoroptions.xml) -replace ' CodeTemplateFileName="[^"]*"', '')

    # Remove user package links with full paths, such as the codebot packages.
    # The installer adds the codebot links again for its install folder. Links
    # relative to the lazarus folder are kept.
    $S = Read-Text packagefiles.xml
    $S = [regex]::Replace($S, '(?s)(<UserPkgLinks[^>]*Count=")\d+("[^>]*>\r?\n)(.*?)([ \t]*</UserPkgLinks>)', {
      param($M)
      $Items = [regex]::Matches($M.Groups[3].Value, '(?s)[ \t]*<Item\d+>.*?</Item\d+>\r?\n') |
        ForEach-Object Value | Where-Object { $_ -notmatch 'Filename Value="([A-Za-z]:\\|\\\\|/)' }
      $N = 0
      $Body = ($Items | ForEach-Object { $N++; $_ -replace '(</?)Item\d+', "`${1}Item$N" }) -join ''
      $M.Groups[1].Value + $N + $M.Groups[2].Value + $Body + $M.Groups[4].Value
    })
    Write-Text packagefiles.xml $S

    # Recent projects, files and packages, history lists and package editor
    # window entries in the environment options
    $S = Read-Text environmentoptions.xml
    $S = $S -replace '(?s)[ \t]*<Recent\b.*?</Recent>\r?\n', ''
    $S = $S -replace '(?s)[ \t]*<History Count="\d+">.*?</History>\r?\n', ''
    $S = $S -replace '(?s)[ \t]*<AutoSave\b[^>]*/>\r?\n', ''
    $S = $S -replace '(?s)[ \t]*<AutoSave\b.*?</AutoSave>\r?\n', ''
    $S = $S -replace '[ \t]*<LastCalledByLazarusFullPath [^>]*/>\r?\n', ''
    $S = $S -replace '(?s)[ \t]*<(PackageEditor_\w*)>.*?</\1>\r?\n', ''
    $S = [regex]::Replace($S, '(<Desktop [^>]*)FormIdCount="\d+"([^>]*>\s*)<FormIdList ([^>]*)/>', {
      param($M)
      $Names = [regex]::Matches($M.Groups[3].Value, 'a\d+="([^"]*)"') |
        ForEach-Object { $_.Groups[1].Value } | Where-Object { -not $_.StartsWith('PackageEditor_') }
      $N = 0
      $Attrs = ($Names | ForEach-Object { $N++; "a$N=`"$_`"" }) -join ' '
      '{0}FormIdCount="{1}"{2}<FormIdList {3}/>' -f $M.Groups[1].Value, $N, $M.Groups[2].Value, $Attrs
    })
    Write-Text environmentoptions.xml $S

    # Replace this computer's folder with a path relative to the lazarus
    # folder, so no user name is left in the settings
    $Base = [regex]::Escape("$PSScriptRoot\")
    Get-ChildItem -File | ForEach-Object {
      $S = Read-Text $_.Name
      if ($S -match $Base) { Write-Text $_.Name ($S -replace $Base, '$$(LazarusDir)..\') }
    }
    $Left = Get-ChildItem -Recurse -File | Select-String -SimpleMatch $env:USERPROFILE -List
    if ($Left) { Write-Warning "Settings still holding ${env:USERPROFILE}: $($Left.Path -join ', ')" }
  }
  finally { Pop-Location }
}
finally { Pop-Location }

# Create the zip archives with 7-Zip
$SevenZip = (Get-Command 7z.exe -ErrorAction SilentlyContinue).Source
if (-not $SevenZip) { $SevenZip = "$env:ProgramFiles\7-Zip\7z.exe" }
if (-not (Test-Path $SevenZip)) { throw 'Could not find 7-Zip' }

$Gh = (Get-Command gh.exe -ErrorAction SilentlyContinue).Source
if (-not $Gh) { $Gh = "$env:ProgramFiles\GitHub CLI\gh.exe" }

Push-Location deploy
try {
  # If either archive is missing, or -Force is used, delete the others,
  # increment the build number and recreate them all
  if ($Force -or -not (Test-Path "fpc.$FpcVersion-*-$Target.zip") -or
      -not (Test-Path "lazarus.$LazVersion-*-$Target.zip")) {
    Remove-Item "fpc.*-$Target.zip", "lazarus.*-$Target.zip" -Force -ErrorAction SilentlyContinue

    $Build = 1
    if (Test-Path ..\buildno-win) { $Build = [int](Get-Content ..\buildno-win) + 1 }
    Set-Content ..\buildno-win $Build

    $FpcZip = "fpc.$FpcVersion-$Build-$Target.zip"
    $LazZip = "lazarus.$LazVersion-$Build-$Target.zip"
    $DllZip = "dlls-$Build-$Target.zip"

    # Every DLL in the local app data bin folder: SDL2, SDL2_image, mpv, assimp 5
    # with its MinGW runtime and zip libraries, OpenSSL and WebView2. The
    # installer extracts them into the same folder.
    $DllDir = "$env:LOCALAPPDATA\bin"
    $Dlls = @(Get-ChildItem $DllDir -File -Filter *.dll -ErrorAction SilentlyContinue)
    foreach ($Name in 'SDL2.dll', 'SDL2_image.dll', 'libmpv-2.dll', 'libassimp-5.dll',
      'libstdc++-6.dll', 'libgcc_s_seh-1.dll', 'libwinpthread-1.dll', 'libminizip-1.dll',
      'zlib1.dll', 'libbz2-1.dll', 'libcrypto-3-x64.dll', 'libssl-3-x64.dll', 'WebView2Loader.dll') {
      if (-not ($Dlls | Where-Object Name -like $Name)) { throw "$DllDir has no $Name" }
    }
    Remove-Item "dlls-*-$Target.zip" -Force -ErrorAction SilentlyContinue
    & $SevenZip a -tzip -mx=9 $DllZip ($Dlls | ForEach-Object FullName) | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "Could not create $DllZip" }
    & $SevenZip a -tzip -mx=9 $FpcZip fpc | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "Could not create $FpcZip" }
    & $SevenZip a -tzip -mx=9 $LazZip lazarus | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "Could not create $LazZip" }

    # Publish the new archives as a GitHub release, then delete the older
    # Windows build releases and their tags
    if (-not (Test-Path $Gh)) { throw 'Could not find gh, the archives were not published' }
    & $Gh release create "build-win-$Build" --repo $Repo --title "Windows Build $Build" `
      --notes "FPC $FpcVersion and Lazarus $LazVersion for x86_64 Windows" $FpcZip $LazZip $DllZip
    if ($LASTEXITCODE -eq 0) {
      & $Gh release list --repo $Repo --limit 100 --json tagName --jq '.[].tagName' |
        Where-Object { $_ -match '^build-win-\d+$' -and $_ -ne "build-win-$Build" } |
        ForEach-Object { & $Gh release delete $_ --repo $Repo --cleanup-tag --yes }
    }
  }
}
finally { Pop-Location }

# Installs only the DLLs and the font the Codebot library depends on, on 64
# bit Windows. It does not install FPC, Lazarus, the library itself or the
# demo projects. Run it in PowerShell with:
#
#   powershell -ExecutionPolicy Bypass -File install-win.ps1
#
# Everything is installed for the current user, so administrator access is
# never needed. The script is one function called at the end, so nothing runs
# if the download is cut short, and it never uses exit, which would close the
# PowerShell window.

function Install-CodebotDependencies {
  $ErrorActionPreference = 'Stop'
  # The progress bar makes downloads very slow in Windows PowerShell 5.1
  $ProgressPreference = 'SilentlyContinue'
  [Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor
    [Net.SecurityProtocolType]::Tls12

  $Repo = 'sysrpl/Codebot.Cross'
  $Target = 'x86_64-win64'
  $Tar = "$env:SystemRoot\System32\tar.exe"
  $DllDir = "$env:LOCALAPPDATA\bin"

  # Asks a yes or no question, the answer defaults to yes
  function Ask($Question) {
    $A = Read-Host "$Question [Y/n]"
    -not ($A -match '^\s*(n|no)\s*$')
  }

  # Explains how to allow a file Microsoft Defender removed
  function Show-DefenderHelp {
    Write-Host
    Write-Host 'Microsoft Defender reported it as a threat. This is a false positive, which'
    Write-Host 'is common for programs made with Free Pascal. To allow it:'
    Write-Host '  1. Open Windows Security, then Virus & threat protection'
    Write-Host '  2. Open Protection history and select the blocked item'
    Write-Host '  3. Choose Actions, then Allow on device or Restore'
    Write-Host '  4. Run this installer again'
    Start-Process windowsdefender: -ErrorAction SilentlyContinue
  }

  # Downloads a file
  function Get-File($Url, $File) {
    Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $File
  }

  # Extracts a zip archive into a folder
  function Expand-Zip($Zip, $Dir) {
    New-Item -ItemType Directory -Force $Dir | Out-Null
    & $Tar -xf $Zip -C $Dir
    if ($LASTEXITCODE -ne 0) { throw "Could not extract $(Split-Path $Zip -Leaf)" }
  }

  Write-Host 'This script will install the DLLs and the font the Codebot library depends'
  Write-Host 'on, on your Windows computer.'
  Write-Host

  if (-not [Environment]::Is64BitOperatingSystem) {
    Write-Host 'This installer needs 64 bit Windows.'
    return
  }
  if (-not (Test-Path $Tar)) {
    Write-Host 'This installer needs Windows 10 version 1803 or later.'
    return
  }

  if (-not (Ask 'Do you want to install the Codebot dependencies?')) {
    Write-Host 'Installation cancelled.'
    return
  }

  $TmpDir = Join-Path ([IO.Path]::GetTempPath()) "codebot-dependencies-$PID"
  $Success = $false
  try {
    if (Test-Path $TmpDir) { Remove-Item -LiteralPath $TmpDir -Recurse -Force }
    New-Item -ItemType Directory -Force $TmpDir | Out-Null

    # Find the newest Windows build. The releases are tagged build-win-N.
    Write-Host
    Write-Host 'Finding the latest build...'
    try {
      $Releases = Invoke-RestMethod -UseBasicParsing "https://api.github.com/repos/$Repo/releases?per_page=100"
    }
    catch { throw 'Could not reach GitHub. Check your internet connection and try again.' }
    $Urls = @($Releases | ForEach-Object { $_.assets } | ForEach-Object { $_.browser_download_url } |
      Where-Object { $_ -match "/download/build-win-\d+/[^/]+-$Target\.zip$" })
    if (-not $Urls) { throw 'Could not find a Windows build on GitHub.' }
    $Build = ($Urls | ForEach-Object { [int]($_ -replace '.*/download/build-win-(\d+)/.*', '$1') } |
      Measure-Object -Maximum).Maximum
    $DllUrl = $Urls | Where-Object { $_ -match "/download/build-win-$Build/dlls-[^/]+$" } |
      Select-Object -First 1
    if (-not $DllUrl) { throw "Build $Build on GitHub is missing the dlls archive." }

    # Download the DLLs
    $Name = Split-Path $DllUrl -Leaf
    Write-Host "Downloading $Name from build $Build"
    try { Get-File $DllUrl "$TmpDir\$Name" }
    catch { throw "Download failed: $DllUrl" }

    # The DLLs go into the local app data bin folder, which is put on the path
    # so programs find them
    Write-Host "Installing the DLLs into $DllDir"
    Expand-Zip "$TmpDir\$Name" $DllDir
    $UserPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if (-not $UserPath) { $UserPath = '' }
    if (-not (($UserPath -split ';') -contains $DllDir)) {
      [Environment]::SetEnvironmentVariable('Path', (@($DllDir) + ($UserPath -split ';' | Where-Object { $_ })) -join ';', 'User')
      Write-Host "Added $DllDir to your path"
    }

    # Material Design Icons font, used by the controls for their glyphs. It is
    # copied from the library this script is in, or downloaded from GitHub, and
    # installed for the current user when there is a newer font.
    $Font = ''
    if ($PSScriptRoot) {
      $Local = Join-Path $PSScriptRoot '..\..\fonts\materialdesignicons.ttf'
      if (Test-Path $Local) { $Font = (Resolve-Path $Local).Path }
    }
    if (-not $Font) {
      $Font = "$TmpDir\materialdesignicons.ttf"
      try { Get-File "https://raw.githubusercontent.com/$Repo/master/fonts/materialdesignicons.ttf" $Font }
      catch {
        Write-Host 'Could not download the Material Design Icons font, skipping it.'
        $Font = ''
      }
    }
    $Fonts = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"
    if ($Font -and (-not (Test-Path "$Fonts\materialdesignicons.ttf") -or
        (Get-FileHash $Font).Hash -ne (Get-FileHash "$Fonts\materialdesignicons.ttf").Hash)) {
      Write-Host
      Write-Host 'Installing the Material Design Icons font'
      New-Item -ItemType Directory -Force $Fonts | Out-Null
      Copy-Item $Font "$Fonts\materialdesignicons.ttf" -Force
      New-Item -Force 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts' | Out-Null
      Set-ItemProperty 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts' `
        'Material Design Icons (TrueType)' "$Fonts\materialdesignicons.ttf"
    }

    # Check every key DLL arrived. Microsoft Defender removes a file it reports
    # as a threat, sometimes a moment after it is written.
    Start-Sleep -Seconds 2
    $Missing = @("$DllDir\SDL2.dll", "$DllDir\libssl-3-x64.dll", "$DllDir\libcrypto-3-x64.dll",
      "$DllDir\libassimp-5.dll") | Where-Object { -not (Test-Path $_) }
    if ($Missing) {
      Write-Host
      Write-Host 'These files are missing, Microsoft Defender may have removed them:' -ForegroundColor Yellow
      $Missing | ForEach-Object { Write-Host "  $_" }
      Show-DefenderHelp
      throw 'Some files are missing.'
    }

    $Success = $true
  }
  catch {
    Write-Host
    Write-Host $_.Exception.Message -ForegroundColor Red
  }
  finally {
    if (Test-Path $TmpDir) { Remove-Item -LiteralPath $TmpDir -Recurse -Force -ErrorAction SilentlyContinue }
  }
  if (-not $Success) { return }

  Write-Host
  Write-Host 'The Codebot dependencies are installed.' -ForegroundColor Green
  Write-Host
  Write-Host "The DLLs are in $DllDir"
  Write-Host 'Open a new terminal or restart Lazarus so programs find them on the path.'
}

Install-CodebotDependencies

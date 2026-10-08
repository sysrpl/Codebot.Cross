# Installs the Free Pascal compiler (FPC), the Lazarus IDE, the Codebot library
# and its DLLs on 64 bit Windows. Run it in PowerShell with:
#
#   irm https://www.getlazarus.org/install-win.ps1 | iex
#
# Everything is installed for the current user, so administrator access is
# never needed. The script is one function called at the end, so nothing runs
# if the download is cut short, and it never uses exit, which would close the
# PowerShell window.

function Install-CodebotPascal {
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

  # Finds the Windows error code of a program Windows refused to start
  function Get-BlockCode($Err) {
    $E = $Err.Exception
    while ($E) {
      if ($E -is [ComponentModel.Win32Exception]) { return $E.NativeErrorCode }
      $E = $E.InnerException
    }
    0
  }

  # Explains how to allow a program Windows blocked
  function Show-BlockHelp($Code, $File) {
    Write-Host
    Write-Host "Windows blocked $File from running." -ForegroundColor Yellow
    switch ($Code) {
      225 {
        Write-Host 'Microsoft Defender reported it as a threat. This is a false positive, which'
        Write-Host 'is common for programs made with Free Pascal. To allow it:'
        Write-Host '  1. Open Windows Security, then Virus & threat protection'
        Write-Host '  2. Open Protection history and select the blocked item'
        Write-Host '  3. Choose Actions, then Allow on device or Restore'
        Write-Host '  4. Run this installer again'
      }
      { $_ -in 4551, 4556, 4557, 4558, 4559, 4560 } {
        Write-Host 'Smart App Control or an application control policy blocked it. To turn'
        Write-Host 'Smart App Control off:'
        Write-Host '  1. Open Windows Security, then App & browser control'
        Write-Host '  2. Open Smart App Control settings and choose Off'
        Write-Host '  3. Run this installer again'
        Write-Host 'On a computer managed by an organization, ask your IT department instead.'
      }
      1260 {
        Write-Host 'A group policy (AppLocker or Software Restriction Policies) blocked it.'
        Write-Host 'This can only be changed by the administrator of this computer.'
      }
      default {
        Write-Host "Windows error $Code. Check Windows Security, Protection history for the file."
      }
    }
    Start-Process windowsdefender: -ErrorAction SilentlyContinue
  }

  # Runs a program and returns its output, explaining how to allow it if
  # Windows refuses to start it. Messages the program writes to stderr are
  # kept as output, Windows PowerShell would otherwise stop on them.
  function Invoke-Program([string]$File, [string[]]$Arguments) {
    $ErrorActionPreference = 'Continue'
    try {
      $Out = & $File @Arguments 2>&1 | ForEach-Object { "$_" }
    }
    catch {
      $Code = Get-BlockCode $_
      if ($Code) { Show-BlockHelp $Code (Split-Path $File -Leaf) }
      throw "Could not run $(Split-Path $File -Leaf)"
    }
    if ($LASTEXITCODE -ne 0) { throw "$(Split-Path $File -Leaf) failed: $($Out -join "`n")" }
    $Out
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

  Write-Host 'This script will install the Free Pascal compiler (FPC), the Lazarus IDE and'
  Write-Host 'the Codebot library on your Windows computer.'
  Write-Host

  if (-not [Environment]::Is64BitOperatingSystem) {
    Write-Host 'This installer needs 64 bit Windows.'
    return
  }
  if (-not (Test-Path $Tar)) {
    Write-Host 'This installer needs Windows 10 version 1803 or later.'
    return
  }

  if (-not (Ask 'Do you want to install FPC and Lazarus?')) {
    Write-Host 'Installation cancelled.'
    return
  }
  $Demos = Ask 'Install the demo projects?'

  # Smart App Control blocks programs which are not signed, which includes
  # FPC, Lazarus and every program compiled with them
  $Sac = (Get-ItemProperty 'HKLM:\SYSTEM\CurrentControlSet\Control\CI\Policy' -ErrorAction SilentlyContinue).VerifiedAndReputablePolicyState
  if ($Sac -eq 1 -or $Sac -eq 2) {
    Write-Host
    Write-Host 'Smart App Control is turned on. It blocks FPC, Lazarus and the programs' -ForegroundColor Yellow
    Write-Host 'you compile with them. To turn it off, open Windows Security, then' -ForegroundColor Yellow
    Write-Host 'App & browser control, Smart App Control settings, and choose Off.' -ForegroundColor Yellow
    if (-not (Ask 'Continue anyway?')) {
      Write-Host 'Installation cancelled.'
      return
    }
  }

  # Controlled folder access blocks programs writing to Documents and Desktop
  try {
    if ((Get-MpPreference -ErrorAction Stop).EnableControlledFolderAccess -eq 1) {
      Write-Host
      Write-Host 'Controlled folder access is turned on. Programs you compile may be blocked' -ForegroundColor Yellow
      Write-Host 'from saving files in Documents or on the Desktop. Programs can be allowed' -ForegroundColor Yellow
      Write-Host 'in Windows Security, Virus & threat protection, Ransomware protection.' -ForegroundColor Yellow
    }
  }
  catch { }

  # Install folder
  $Default = "$env:USERPROFILE\Development\Pascal"
  $Prompt = 'Install FPC and Lazarus into this folder'
  while ($true) {
    Write-Host
    $InstallDir = Read-Host "$Prompt [$Default]"
    if (-not $InstallDir.Trim()) { $InstallDir = $Default }
    $InstallDir = $InstallDir.Trim().Trim('"').TrimEnd('\')
    if ($InstallDir.StartsWith('~')) { $InstallDir = $env:USERPROFILE + $InstallDir.Substring(1) }
    try { $InstallDir = [IO.Path]::GetFullPath($InstallDir) }
    catch {
      $Prompt = 'That is not a valid folder name. Install FPC and Lazarus into this folder'
      continue
    }
    $HaveFpc = $false
    if ((Test-Path "$InstallDir\fpc\bin\$Target\fpc.exe") -and (Test-Path "$InstallDir\lazarus\lazbuild.exe")) {
      # FPC and Lazarus were installed before, so the download is skipped and
      # only the Codebot library, DLLs and demos are added or updated
      $HaveFpc = $true
      break
    }
    if ((Test-Path "$InstallDir\fpc") -or (Test-Path "$InstallDir\lazarus")) {
      $Prompt = "An incomplete fpc or lazarus folder exists in $InstallDir. Please pick another folder"
      continue
    }
    break
  }

  if ($HaveFpc) {
    Write-Host
    Write-Host "FPC and Lazarus are already installed in $InstallDir, skipping the"
    Write-Host 'download. The Codebot library, DLLs and demos will be added or updated.'
  }

  $FpcDir = "$InstallDir\fpc"
  $LazDir = "$InstallDir\lazarus"
  $FpcBin = "$FpcDir\bin\$Target"
  $CodebotDir = "$InstallDir\Libraries\Codebot"
  $DemosDir = "$InstallDir\Projects\Demos"
  $TmpDir = "$InstallDir\install.tmp"

  # What this run created, so a failed install can be removed
  $Made = @{}
  $Success = $false

  # Puts back a folder an update replaced, or removes a folder this script
  # downloaded when there was none before
  function Restore-Folder($Dir, $WasMade) {
    if (Test-Path "$Dir.old") {
      Remove-Item -LiteralPath $Dir -Recurse -Force -ErrorAction SilentlyContinue
      Move-Item -LiteralPath "$Dir.old" $Dir
    }
    elseif ($WasMade) {
      Remove-Item -LiteralPath $Dir -Recurse -Force -ErrorAction SilentlyContinue
    }
  }

  # Downloads the default branch of a GitHub repository into a folder, or
  # updates the folder when that branch has a newer commit. The commit is kept
  # in a .commit file in the folder to compare with next time. A folder holding
  # a git clone is never replaced. Returns true only when the folder was
  # downloaded.
  function Update-Folder($Title, $Source, $Dir, $Key) {
    Write-Host
    if (Test-Path "$Dir\.git") {
      Write-Host "$Dir is a git clone, keeping it."
      return $false
    }
    # Asking for archive/HEAD.zip, the zip of the default branch, makes GitHub
    # answer with a redirect to the zip of that branch's latest commit. Only
    # the headers are requested and the redirect is not followed.
    $Remote = ''
    try {
      $Req = [Net.HttpWebRequest]::Create("https://github.com/$Source/archive/HEAD.zip")
      $Req.Method = 'HEAD'
      $Req.AllowAutoRedirect = $false
      $Req.UserAgent = 'install-win'
      $Res = $Req.GetResponse()
      try { $Location = $Res.Headers['Location'] } finally { $Res.Close() }
      if ($Location -match '/zip/([0-9a-f]+)$') { $Remote = $Matches[1] }
    }
    catch { }
    if (Test-Path $Dir) {
      $Local = ''
      if (Test-Path "$Dir\.commit") { $Local = (Get-Content "$Dir\.commit" -Raw).Trim() }
      if (-not $Remote) {
        Write-Host "Could not check GitHub for a newer $Title, keeping $Dir."
        return $false
      }
      if ($Remote -eq $Local) {
        Write-Host "$Title is up to date (commit $($Local.Substring(0, 7)))."
        return $false
      }
      Write-Host "Updating $Title to commit $($Remote.Substring(0, 7))"
    }
    else {
      if (-not $Remote) { throw "Could not find $Title on GitHub. Check your internet connection and try again." }
      Write-Host "Downloading $Title (commit $($Remote.Substring(0, 7)))"
      $Made[$Key] = $true
    }

    # Download that exact commit, so .commit matches what was installed
    $Zip = "$TmpDir\$Key.zip"
    Get-File "https://github.com/$Source/archive/$Remote.zip" $Zip

    # The zip holds a single top folder, extract it and move it into place. An
    # existing folder is kept as .old until the new one is in place.
    $Out = "$TmpDir\$Key"
    Expand-Zip $Zip $Out
    New-Item -ItemType Directory -Force (Split-Path $Dir) | Out-Null
    if (Test-Path "$Dir.old") { Remove-Item -LiteralPath "$Dir.old" -Recurse -Force }
    if (Test-Path $Dir) { Move-Item -LiteralPath $Dir "$Dir.old" }
    Move-Item -LiteralPath (Get-ChildItem $Out -Directory | Select-Object -First 1).FullName $Dir
    Set-Content "$Dir\.commit" $Remote
    $true
  }

  $MadeDir = -not (Test-Path $InstallDir)
  try {
    New-Item -ItemType Directory -Force $InstallDir | Out-Null
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
    $BuildUrls = $Urls | Where-Object { $_ -match "/download/build-win-$Build/" }
    $FpcUrl = $BuildUrls | Where-Object { $_ -match '/fpc\.[^/]+$' }
    $LazUrl = $BuildUrls | Where-Object { $_ -match '/lazarus\.[^/]+$' }
    $DllUrl = $BuildUrls | Where-Object { $_ -match '/dlls-[^/]+$' }
    if (-not $FpcUrl -or -not $LazUrl -or -not $DllUrl) {
      throw "Build $Build on GitHub is missing the fpc, lazarus or dlls archive."
    }

    # Download and extract FPC and Lazarus for a new install
    $Downloads = @($DllUrl)
    if (-not $HaveFpc) { $Downloads = @($FpcUrl, $LazUrl, $DllUrl) }
    Write-Host "Build $Build will be downloaded into $InstallDir"
    foreach ($Url in $Downloads) {
      $Name = Split-Path $Url -Leaf
      Write-Host "Downloading $Name"
      try { Get-File $Url "$TmpDir\$Name" }
      catch { throw "Download failed: $Url" }
    }

    if (-not $HaveFpc) {
      $Made['fpc'] = $true
      foreach ($Url in $FpcUrl, $LazUrl) {
        $Name = Split-Path $Url -Leaf
        Write-Host "Extracting $Name"
        Expand-Zip "$TmpDir\$Name" $InstallDir
      }
    }

    # The DLLs go into the local app data bin folder, which is put on the path
    # so programs find them
    Write-Host "Installing the DLLs into $DllDir"
    Expand-Zip "$TmpDir\$(Split-Path $DllUrl -Leaf)" $DllDir
    $UserPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if (-not (($UserPath -split ';') -contains $DllDir)) {
      [Environment]::SetEnvironmentVariable('Path', (@($DllDir) + ($UserPath -split ';' | Where-Object { $_ })) -join ';', 'User')
      Write-Host "Added $DllDir to your path"
    }

    # Codebot library
    Update-Folder 'the Codebot library' $Repo $CodebotDir codebot | Out-Null

    # Material Design Icons font, used by the controls for their glyphs. It is
    # installed for the current user and copied again when the library has a
    # newer font.
    $Font = "$CodebotDir\fonts\materialdesignicons.ttf"
    $Fonts = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"
    if ((Test-Path $Font) -and (-not (Test-Path "$Fonts\materialdesignicons.ttf") -or
        (Get-FileHash $Font).Hash -ne (Get-FileHash "$Fonts\materialdesignicons.ttf").Hash)) {
      Write-Host
      Write-Host 'Installing the Material Design Icons font'
      New-Item -ItemType Directory -Force $Fonts | Out-Null
      Copy-Item $Font $Fonts -Force
      New-Item -Force 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts' | Out-Null
      Set-ItemProperty 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts' `
        'Material Design Icons (TrueType)' "$Fonts\materialdesignicons.ttf"
    }

    # Demo projects from the Codebot.Demos repository
    if ($Demos) { Update-Folder 'the demo projects' sysrpl/Codebot.Demos $DemosDir demos | Out-Null }

    # Files downloaded by this script are not marked as coming from the
    # internet, this removes the mark from anything which has it anyway
    Get-ChildItem $FpcDir, $LazDir -Recurse -File -ErrorAction SilentlyContinue | Unblock-File

    # Check every key file arrived. Microsoft Defender removes a file it
    # reports as a threat, sometimes a moment after it is written.
    Start-Sleep -Seconds 2
    $Missing = @("$FpcBin\fpc.exe", "$FpcBin\ppcx64.exe", "$FpcBin\fpcmkcfg.exe", "$LazDir\lazarus.exe",
      "$LazDir\startlazarus.exe", "$LazDir\lazbuild.exe", "$LazDir\lazarus-run.vbs", "$LazDir\lazarus-run.bat",
      "$DllDir\SDL2.dll", "$DllDir\libssl-3-x64.dll", "$DllDir\libcrypto-3-x64.dll", "$DllDir\libassimp-5.dll") |
      Where-Object { -not (Test-Path $_) }
    if ($Missing) {
      Write-Host
      Write-Host 'These files are missing, Microsoft Defender may have removed them:' -ForegroundColor Yellow
      $Missing | ForEach-Object { Write-Host "  $_" }
      Show-BlockHelp 225 'a downloaded file'
      throw 'Some files are missing.'
    }

    # Set up the compiler configuration and path, as lazarus-run.bat does
    $env:PPC_CONFIG_PATH = $FpcBin
    $env:Path = "$FpcBin;$DllDir;$env:Path"
    Remove-Item "$FpcBin\fpc.cfg" -Force -ErrorAction SilentlyContinue
    Invoke-Program "$FpcBin\fpcmkcfg.exe" @('-d', "basepath=$FpcDir", '-o', "$FpcBin\fpc.cfg") | Out-Null
    $Version = Invoke-Program "$FpcBin\fpc.exe" @('-iV')
    Write-Host
    Write-Host "Free Pascal $Version is working"

    # Register the codebot packages with Lazarus. The IDE already has them
    # built in, the links tell it where their source is.
    Write-Host
    Write-Host 'Adding the codebot packages to Lazarus'
    $LazBuild = "$LazDir\lazbuild.exe"
    $LazOptions = @("--lazarusdir=$LazDir", "--pcp=$LazDir\config")
    Get-ChildItem "$CodebotDir\source" -Directory | ForEach-Object {
      $Lpk = "$($_.FullName)\$($_.Name).lpk"
      if (Test-Path $Lpk) { Invoke-Program $LazBuild ($LazOptions + @('--add-package-link', $Lpk)) | Out-Null }
    }

    # Start Menu shortcuts for the current user
    $Menu = [Environment]::GetFolderPath('Programs')
    $Shell = New-Object -ComObject WScript.Shell
    $Link = $Shell.CreateShortcut("$Menu\Lazarus.lnk")
    $Link.TargetPath = "$env:SystemRoot\System32\wscript.exe"
    $Link.Arguments = "`"$LazDir\lazarus-run.vbs`""
    $Link.WorkingDirectory = $LazDir
    $Link.IconLocation = "$LazDir\images\icons\lazarus.ico,0"
    $Link.Description = 'Lazarus IDE powered by Free Pascal'
    $Link.Save()
    $Link = $Shell.CreateShortcut("$Menu\Free Pascal Terminal.lnk")
    $Link.TargetPath = "$env:SystemRoot\System32\cmd.exe"
    $Link.Arguments = "/k `"`"$FpcDir\setup.bat`"`""
    $Link.WorkingDirectory = $env:USERPROFILE
    $Link.Description = 'Open a new terminal with the fpc program made available'
    $Link.Save()

    # Finished, nothing to clean up. Remove the folders replaced by updates.
    $Success = $true
    foreach ($Dir in $CodebotDir, $DemosDir) {
      if (Test-Path "$Dir.old") { Remove-Item -LiteralPath "$Dir.old" -Recurse -Force }
    }
  }
  catch {
    Write-Host
    Write-Host $_.Exception.Message -ForegroundColor Red
  }
  finally {
    if (Test-Path $TmpDir) { Remove-Item -LiteralPath $TmpDir -Recurse -Force -ErrorAction SilentlyContinue }
    if (-not $Success) {
      # Remove what was installed. The fpc and lazarus folders are only removed
      # when this script downloaded them, so an existing install is never removed.
      Write-Host
      Write-Host 'Installation failed. Removing the partly installed files...'
      if ($Made['fpc']) {
        Remove-Item -LiteralPath $FpcDir, $LazDir -Recurse -Force -ErrorAction SilentlyContinue
      }
      Restore-Folder $DemosDir $Made['demos']
      Restore-Folder $CodebotDir $Made['codebot']
      foreach ($Dir in "$InstallDir\Projects", "$InstallDir\Libraries") {
        if ((Test-Path $Dir) -and -not (Get-ChildItem $Dir -Force)) { Remove-Item -LiteralPath $Dir }
      }
      if ($MadeDir -and (Test-Path $InstallDir) -and -not (Get-ChildItem $InstallDir -Force)) {
        Remove-Item -LiteralPath $InstallDir
      }
    }
  }
  if (-not $Success) { return }

  Write-Host
  Write-Host 'Installation complete.' -ForegroundColor Green
  Write-Host
  Write-Host "FPC and Lazarus are installed in $InstallDir"
  Write-Host "The Codebot library is in $CodebotDir"
  if ($Demos) { Write-Host "The demo projects are in $DemosDir" }
  Write-Host "The DLLs are in $DllDir"
  Write-Host
  Write-Host 'Two shortcuts were added to your Start Menu:'
  Write-Host '  Lazarus               - starts the Lazarus IDE'
  Write-Host '  Free Pascal Terminal  - opens a terminal with the fpc compiler available'

  # Optionally build the demo projects, then optionally run the mega demo
  if ($Demos -and (Test-Path $DemosDir)) {
    Write-Host
    if (Ask 'Do you want to build the demo projects?') {
      $Log = [IO.Path]::GetTempFileName()
      $Failed = @()
      # Build messages on stderr go to the log, they must not stop the script
      $ErrorActionPreference = 'Continue'
      Write-Host
      Get-ChildItem $DemosDir -Recurse -Filter *.lpi | Where-Object FullName -notmatch '\\backup\\' |
        Sort-Object FullName | ForEach-Object {
          Write-Host "Building $($_.BaseName)"
          & $LazBuild @LazOptions -q $_.FullName *>> $Log
          if ($LASTEXITCODE -ne 0) {
            Write-Host "  $($_.BaseName) could not be built"
            $Failed += $_.BaseName
          }
        }
      Write-Host
      if ($Failed) {
        Write-Host "Some demo projects could not be built: $($Failed -join ', ')"
        Write-Host "The build messages are in $Log"
      }
      else {
        Remove-Item $Log -Force
        Write-Host 'The demo projects were built.'
        $MegaDemo = "$DemosDir\MegaDemo\megademo.exe"
        if (Test-Path $MegaDemo) {
          Write-Host
          if (Ask 'Do you want to run the mega demo?') {
            try { Start-Process $MegaDemo -WorkingDirectory (Split-Path $MegaDemo) }
            catch {
              $Code = Get-BlockCode $_
              if ($Code) { Show-BlockHelp $Code 'megademo.exe' } else { Write-Host $_.Exception.Message }
            }
          }
        }
      }
    }
  }
}

Install-CodebotPascal

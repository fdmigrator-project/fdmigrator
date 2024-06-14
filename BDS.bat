@echo off
setlocal enableextensions enabledelayedexpansion

set SELF_NAME=%~n0
set SELF_VS=2.1

set "PRJ_ROOT_KEY=USR_PRJ"

:: Current dir without trailing backslash
set WORK_DIR=%~dp0
IF %WORK_DIR:~-1%==\ SET WORK_DIR=%WORK_DIR:~0,-1%

:: default IDE Paths
set "TESTINSIGHT_DIR=%LOCALAPPDATA%\Programs\TestInsight\Source"
set "DUNITX=%USERPROFILE%\Documents\DUnitX"
set "DelphiMocks=%USERPROFILE%\Documents\Delphi-Mocks"

:: default PRJ vars
set "PRJ_DIR=%WORK_DIR%"
set "VENDOR_BIN=%PRJ_DIR%\vendor\bin"
set "VENDOR_LIB=%PRJ_DIR%\vendor\lib"
set "PRJ_OUT_DIR=%PRJ_DIR%\out"


:: adjust dir where IDE offers to save projects by default
set "BDSPROJECTSDIR=%PRJ_DIR%"
set "BDSUSERDIR=%PRJ_DIR%"

for %%i IN ("%PRJ_DIR%") DO set "PRJ_NAME=%%~ni"

:: load project specific settings so we can override defaults
call "%WORK_DIR%\project.cmd"

:: add vendor_bin to path, so you can install your vendor pkg
set PATH=%PATH%;%VENDOR_BIN%

:: create output dir
mkdir "%PRJ_OUT_DIR%" 2>nul

if (%PRJ_IDE%)==() goto :ERR_PRJ_IDE_NOT_DEFINED
:: select ide version from PRJ_IDE: remove last digit & remove D prefix
SET IDE_VER=%PRJ_IDE:~0,-1%
SET IDE_VER=%IDE_VER:~1%

:: find delphi registry key
set "delphi_reg="
if %IDE_VER% LEQ 7 (
  set "delphi_reg=Borland\Delphi\%IDE_VER%.0"
) else (
  if !IDE_VER! LEQ 14 (
    set /A IDE_VER-=6
    set "delphi_reg=Borland\BDS\!IDE_VER!.0"
  ) else (
    if !IDE_VER! LEQ 19 (
      set /A IDE_VER-=7
      set "delphi_reg=Embarcadero\BDS\!IDE_VER!.0"
    ) else (
      set /A IDE_VER-=6
      set "delphi_reg=Embarcadero\BDS\!IDE_VER!.0"
    )
  )
)

if (%delphi_reg%)==() goto :ERR_IDE_VS_NOT_FOUND

:: Get BDS root path & BDS app from Windows Registry
for /f "tokens=2*" %%a in ('reg query "HKCU\Software\%delphi_reg%" /v "App" 2^>nul') do set "BDSApp=%%~b"
for /f "tokens=2*" %%a in ('reg query "HKCU\Software\%delphi_reg%" /v "RootDir" 2^>nul') do set "BDS=%%~b"
set BDS=!BDS:~0,-1!

if ("%BDSApp%")==("") goto :ERR_IDE_VS_NOT_FOUND

call :BANNER

:: select command

if (%1)==(i) (
call :INSPECT
exit /B 0
)

if (%1)==(env) (
call :ENV
exit /B 0
) 

if (%1)==(clean) (
call :CLEAN
exit /B 0
) 

if (%1)==(make) (
call :MAKE
exit /B 0
) 

if (%1)==(build) (
call :BUILD
exit /B 0
) 

if (%1)==(inno) (
iscc /Qp .\AppSetup.iss /DBuildConfig=Release
exit /B 0
) 

:: default command is start ide
call :START
goto :eof

:: Error handling
:ERR_PRJ_IDE_NOT_DEFINED
echo PRJ_IDE variable not defined
goto :eof

:ERR_IDE_VS_NOT_FOUND
echo IDE version not found, check PRJ_IDE variable and ensure Delphi is installed
goto :eof

:: Available commands
:BANNER
echo %SELF_NAME% v%SELF_VS%
exit /B 0

:INSPECT
echo    project
echo ------------
set PRJ_
set VENDOR
echo    BDS
echo ------------
SET BDS
exit /B 0

:INNO
iscc /Qp .\AppSetup.iss /DBuildConfig=Release
exit /B 0

:MAKE
if (%2)==() (
  set usecfg=Debug
) else (
  set "usecfg=%2" 
)
if exist "%BDS%\bin" ( 
  call "%BDS%\bin\rsvars" > nul
  call msbuild "%PRJ_LOAD%" /t:make /p:Config=%usecfg% /p:Platform=Win32
)
exit /B 0

:BUILD
if (%2)==() (
  set usecfg=Debug
) else (
  set "usecfg=%2" 
)
if exist "%BDS%\bin" ( 
  call "%BDS%\bin\rsvars" > nul
  call msbuild "%PRJ_LOAD%" /t:Build /p:Config=%usecfg% /p:Platform=Win32
)
exit /B 0

:CLEAN
if (%2)==() (
  set usecfg=Debug
) else (
  set "usecfg=%2" 
)

if exist "%BDS%\bin" ( 
  call "%BDS%\bin\rsvars" > nul
  call msbuild "%PRJ_LOAD%" /t:Clean /p:Config=%usecfg% /p:Platform=Win32
)
exit /B 0

:START
start "BDS" "%BDSApp%" -idecaption="%PRJ_NAME%" -r"%PRJ_ROOT_KEY%\%PRJ_NAME%" "%PRJ_LOAD%" 
exit /B 0

:ENV
echo setup developer env
robocopy "%PRJ_DIR%\runenv\_shared" "%PRJ_OUT_DIR%" /E /NJH /NJS /NFL /NP /NDL
robocopy "%PRJ_DIR%\runenv\deve" "%PRJ_OUT_DIR%" /E /NJH /NJS /NFL /NP /NDL
exit /B 0

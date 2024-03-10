@echo off
setlocal enableextensions enabledelayedexpansion

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
set "PRJ_OUT_DIR=%PRJ_DIR%\out"
set "RUN_DEVE_DIR=%PRJ_OUT_DIR%"
set "PRJ_APPDATA_DIR=%RUN_DEVE_DIR%"
set "VENDOR_BIN=%PRJ_DIR%\vendor\bin"
set "VENDOR_LIB=%PRJ_DIR%\vendor\lib"


:: adjust dir where IDE offers to save projects by default
set "BDSPROJECTSDIR=%PRJ_DIR%"
set "BDSUSERDIR=%PRJ_DIR%"


for %%i IN ("%PRJ_DIR%") DO set "PRJ_NAME=%%~ni"

call "%WORK_DIR%\project.cmd"

:: add vendor_bin to path, so you can install your vendor pkg
set PATH=%PATH%;%VENDOR_BIN%

:: create some write dirs
mkdir "%PRJ_OUT_DIR%" 2>nul
mkdir "%PRJ_APPDATA_DIR%" 2>nul

if (%1)==(i) (
:: inspect var
echo project
echo ------------
set PRJ_
set VENDOR
echo    BDS
echo ------------
SET BDS
exit /B 0
)

if (%1)==(e) (
echo setup dev env
robocopy "%PRJ_DIR%\runenv\_shared" "%RUN_DEVE_DIR%" /E /NJH /NJS /NFL /NP /NDL
robocopy "%PRJ_DIR%\runenv\deve" "%RUN_DEVE_DIR%" /E /NJH /NJS /NFL /NP /NDL
exit /B 0
) 

if (%1)==(inno) (
iscc /Qp .\AppSetup.iss /DBuildConfig=Release
exit /B 0
) 

:: start delphi ide
start "BDS" bds.exe -idecaption="%PRJ_NAME%" -r"%PRJ_ROOT_KEY%\%PRJ_NAME%%" "%PRJ _LOAD%" 



@echo off
echo Compiling Lazarus project for x86_64
rem delete binary file
if exist "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe" (
  del /q "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe"
)

rem compile lazarus project
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=x86_64 --build-mode=Release --no-write-project "C:\Pascal\Give-Your-Voice\GiveYourVoice.lpi" >NUL 2>NUL

rem check if binary was build
if not exist "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe" (
  echo COMPILATION ERROR FOR TARGET x86_64
  pause
  exit /b
)
echo x86_64 build

rem invoke InnoSetup
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Qp "C:\Pascal\Give-Your-Voice\ReleaseTools\win\ScriptForWin64.iss"

echo.

echo Compiling Lazarus project for i386
rem delete binary file
if exist "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe" (
  del /q "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe"
)

rem compile lazarus project
"C:\lazarus\lazbuild.exe" --build-all --quiet --widgetset=win32 --cpu=i386 --build-mode=Release --no-write-project "C:\Pascal\Give-Your-Voice\GiveYourVoice.lpi" >NUL 2>NUL

rem check if binary was build
if not exist "C:\Pascal\Give-Your-Voice\Binary\GiveYourVoice.exe" (
  echo COMPILATION ERROR FOR TARGET i386
  pause
  exit /b
)
echo x86_64 build

rem invoke InnoSetup
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Qp "C:\Pascal\Give-Your-Voice\ReleaseTools\win\ScriptForWin32.iss"

pause

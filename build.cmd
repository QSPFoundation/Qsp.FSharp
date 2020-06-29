@echo off
cls

IF EXIST ".paket\paket.exe" (
  REM Do one thing
) ELSE (
  .paket\paket.bootstrapper.exe
)
.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
if errorlevel 1 (
  REM pause
)
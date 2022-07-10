@echo off

clang compile.c && a.exe bin/tildebackend
if ERRORLEVEL 1 exit /b 1

wsl gcc compile.c
wsl ./a.out bin/tildebackend

pause

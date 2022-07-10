@echo off

clang compile.c -DRELEASE_BUILD && a.exe bin/tildebackend
if ERRORLEVEL 1 exit /b 1

wsl gcc -DRELEASE_BUILD compile.c
wsl ./a.out bin/tildebackend

pause

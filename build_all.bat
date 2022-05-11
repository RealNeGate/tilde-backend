@echo off

copy W:\Workspace\tilde-backend\src\tb\tb.h W:\Workspace\Cuik\src\back\

clang compile.c && a.exe W:/Workspace/Cuik/tildebackend
if ERRORLEVEL 1 exit /b 1

wsl gcc compile.c
wsl ./a.out /mnt/w/workspace/cuik/tildebackend

@echo off

copy W:\Workspace\tilde-backend\src\tb\tb.h W:\Workspace\Cuik\src\back\

clang -DNEGATE compile.c && a.exe W:/Workspace/Cuik/tildebackend
if ERRORLEVEL 1 exit /b 1

wsl gcc -DNEGATE compile.c && ./a.out W:/Workspace/Cuik/tildebackend

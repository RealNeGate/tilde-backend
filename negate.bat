@echo off

rem clang compile.c && a.exe bin/tildebackend
rem if ERRORLEVEL 1 exit /b 1

rem wsl gcc compile.c
rem wsl ./a.out bin/tildebackend

rem copy bin\tildebackend.lib W:\Workspace\Cuik\deps\
rem copy bin\tildebackend.a W:\Workspace\Cuik\deps\
copy include\tb.h W:\Workspace\Cuik\include\

pause

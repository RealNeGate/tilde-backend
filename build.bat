@echo off

cl /Fe:compile /DBUILD_FUZZER compile.c
if ERRORLEVEL 1 exit /b 1

compile
if ERRORLEVEL 1 exit /b 1

build\fuzzer.exe
if ERRORLEVEL 1 exit /b 1

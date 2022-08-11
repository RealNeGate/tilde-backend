@echo off
tup

IF %ERRORLEVEL% EQU 0 (
	copy bin\tildebackend.lib W:\Workspace\Cuik\deps\
	copy include\tb.h W:\Workspace\Cuik\include\

	cd W:\Workspace\Cuik && tup
)
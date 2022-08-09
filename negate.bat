@echo off
tup
copy bin\tildebackend.lib W:\Workspace\Cuik\deps\
copy include\tb.h W:\Workspace\Cuik\include\

cd W:\Workspace\Cuik && tup
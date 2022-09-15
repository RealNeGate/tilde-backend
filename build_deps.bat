@echo off

IF NOT EXIST deps\luajit\src\lua51.lib (
    mkdir deps\luajit\src
    cd deps\luajit\src

    msvcbuild.bat
)

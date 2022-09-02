@echo off

rem Make the output binary directory and fill it with the CMake bullshit
IF NOT EXIST deps\mimalloc\out (
    mkdir deps\mimalloc\out
    cd deps\mimalloc\out

    rem Actually build, we really should get rid of the Cmake & MSbuild dependency
    cmake -DMI_BUILD_TESTS=OFF -DMI_BUILD_OBJECT=OFF -DMI_BUILD_SHARED=OFF ../
    msbuild libmimalloc.sln -p:Configuration=Release
    copy Release\mimalloc-static.lib ..\..\
)

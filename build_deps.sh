#!/bin/bash

if [ ! -f "bin/luajit" ]
then
    cd deps/luajit/src
    make CC=clang BUILDMODE=static libluajit.a
    cd ../../..
    
    # extract objects into bin/bash
    mkdir -p bin/luajit
    cd bin/luajit
    ar -x ../../deps/luajit/src/libluajit.a
    cd ../..
fi

#!/bin/sh

cd lua5.1-tests
export LUA_PATH="./?.lua;;"
export LUA_INIT="package.path = '?;'..package.path"
export LUA_INTERPRETER="luajit_$(uname)"
../LuaJIT-2.0.5//src/luajit all.lua

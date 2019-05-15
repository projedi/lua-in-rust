#!/bin/sh

cd lua5.1-tests
export LUA_PATH="./?.lua;;"
export LUA_INIT="package.path = '?;'..package.path"
export LUA_INTERPRETER="lua_$(uname)"
../lua-5.1.5/src/lua all.lua

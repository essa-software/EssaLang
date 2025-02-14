#!/bin/sh

cd runtime
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -GNinja -DCMAKE_INSTALL_PREFIX=$PREFIX ..
ninja
ninja install

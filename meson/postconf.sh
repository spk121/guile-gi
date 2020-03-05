#!/bin/sh
mkdir -p $MESON_BUILD_ROOT/tools
mkdir -p $MESON_BUILD_ROOT/test
for tool in 'gdb-guile' 'gdb-test' 'lcov.sh' 'run-guile' 'run-test' 'uninstalled-env' 'uninstalled-test-env' 'valgrind.sh'; do
    mv $MESON_BUILD_ROOT/$tool $MESON_BUILD_ROOT/tools/$tool
done
mv $MESON_BUILD_ROOT/config.scm $MESON_SOURCE_ROOT/module/gi/config.scm

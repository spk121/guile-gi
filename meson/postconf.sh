#!/bin/sh
mkdir -p $MESON_BUILD_ROOT/test # required for tests
chmod +w $MESON_BUILD_ROOT/gitestmacros.h # required on Guix
mv $MESON_BUILD_ROOT/config.scm $MESON_SOURCE_ROOT/module/gi/config.scm # required for build

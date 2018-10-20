#!/bin/sh

gcc -O0 -ggdb -g3 -Wall `pkg-config gobject-2.0 guile-2.2 --cflags --libs` closures.c -o closures

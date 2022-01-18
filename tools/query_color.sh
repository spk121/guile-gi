#!/bin/sh
# -*- mode: sh; coding: us-ascii; indent-tabs-mode: nil; -*-

# This searches this computer's terminfo database for terminals that
# have 8 or more colors and a 'setaf' terminfo property.  I look for
# the %p1%d string in setaf because that is an ANSI color escape
# string.

TERMS=$(ls -1 /usr/share/terminfo/*/* | xargs basename -a)
echo "{"
for t in $TERMS
do
    C=$(tput -T"$t" colors)
    if [ "$C" -gt 7 ] ; then
        F=$(tput -T"$t" setaf)
        if echo "$F" | sed -n '/%p1%d[;m]/q0' ; then
            echo "\"$t\","
        fi
    fi
done
echo "}"

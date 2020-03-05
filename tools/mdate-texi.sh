#!/bin/sh
MTIME=$(stat $1 -c '%Y')
VERSION=$2

set $(date '+%-d %B %Y' --date=@$MTIME)
echo '@set UPDATED' $1 $2 $3
echo '@set UPDATED-MONTH' $2 $3
echo '@set EDITION' $VERSION
echo '@set VERSION' $VERSION

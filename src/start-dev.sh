#!/bin/sh

set -e

SNAME=$(hostname)

if [ ! "$1" = "" ]; then
    SNAME="$1"
fi

cd `dirname $0`
for i in *.erl; do
    erlc "$i"
done
exec erl \
    -boot start_sasl \
    -sname "$SNAME" \
    -s crypto \
    -s eggpd

#!/bin/sh

docker run -v $(pwd):/mnt --rm -it schemers/gambit:head gsc -target js -exe /mnt/$1
tail -n +2 $(basename $1 .scm)
rm -f $(basename $1 .scm)

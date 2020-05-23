#!/bin/bash

if xhost >& /dev/null ; then
  # NOTE - On OS X, you have to have enabled network connections in XQuartz!
  xhost + 127.0.0.1
fi

workdir="$PWD"

docker run \
       --ipc host \
       --interactive --tty --rm \
       --workdir $workdir \
       --volume $workdir:$workdir \
       -e DISPLAY=host.docker.internal:0 \
       sritchie/mit-scheme "$@"

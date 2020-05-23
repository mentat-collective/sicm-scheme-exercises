#!/bin/bash

# We have to sleep for a tiny amount, since Docker initializes rlwrap
# asynchronously https://github.com/sflyr/docker-sqlplus/pull/2
sleep 0.1
exec rlwrap "${@}"

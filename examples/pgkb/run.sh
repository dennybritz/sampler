#! /usr/bin/env bash

YOURKIT_AGENT_PATH="/Applications/YourKit_Java_Profiler_2013_build_13062.app/bin/mac/libyjpagent.jnilib"
cd "$(dirname $0)/../..";
BASEDIR=`pwd`

export SBT_OPTS="-agentpath:$YOURKIT_AGENT_PATH -Xmx4g"

sbt "run --variables $BASEDIR/examples/pgkb/variables.txt --weights $BASEDIR/examples/pgkb/weights.txt \
  --factors $BASEDIR/examples/pgkb/factors.txt -o $BASEDIR/target/output -i 100"
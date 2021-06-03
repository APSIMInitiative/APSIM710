#!/bin/bash

mkdir tgz
docker build . -t apsim
docker run --rm -v `pwd`/tgz:/opt/apsim/ apsim BuildApsim.sh
docker image rm -f apsim



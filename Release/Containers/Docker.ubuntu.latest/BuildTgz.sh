#!/bin/bash


rm -rf tgz && mkdir tgz

docker build . -t apsim
docker run --rm -v `pwd`/tgz:/out apsim BuildApsim.sh
docker image rm -f apsim


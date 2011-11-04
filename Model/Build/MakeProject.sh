#! /bin/sh

cd $APSIM/Model/$1

if  [ -f Makefile ]; then
  make $2 
fi


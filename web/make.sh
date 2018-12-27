#!/bin/sh

if [ "$1" = "clean" ]; then
  if [ "$2" = "all" ]; then
    make -C .. clean;
  fi
  if [ -e converter.js ]; then
    make clean;
    command="rm converter.js";
    echo $command;
    `$command`;
  fi
else
  make -C .. bcl;
  make
fi

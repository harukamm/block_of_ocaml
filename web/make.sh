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
  make;
  if [ ! -e converter.js ]; then
    command="js_of_ocaml +weak.js +toplevel.js +dynlink.js converter";
    echo $command;
    `$command`;
  fi
fi

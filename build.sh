#!/usr/bin/env bash

distFolder='dist/build/tvheadend-frontend-output/tvheadend-frontend-output.jsexe'

operations="
  cabal build # build the project
  echo
  echo Copying html file to output folder...;
  cp index.html output/.
  echo Copying build JavaScript files to output folder...;
  cp "$distFolder/{rts,lib,out,runmain}.js" output/.
  echo Done.
"
# the 'd' argument to entr should make the project
# rebuild if new files are added to the already tracked directories
# the 'c' argument to entr clear the screen on each rebuild
find . -path ./dist -prune -o -regextype posix-extended -regex '.*\.(hs|cabal|html)$' -print | entr -cd sh -c "$operations"

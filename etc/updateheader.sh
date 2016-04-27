#!/bin/bash

OLDHEADERSIZE=18

for FILE in $(find . -name '*.scala'); do
  # Remove old header
  sed -i 1,${OLDHEADERSIZE}d ${FILE}
  
  # Create a new file with the new header
  cat etc/header $FILE > $FILE.new
  
  # Replace the old file with the new one
  mv $FILE.new $FILE
done

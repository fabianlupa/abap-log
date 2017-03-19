#!/bin/bash

# Remove generation date footer so it does not show up as a diff
for file in $(find $(dirname "$(readlink -f "$0")") -name '*.html');
do
  sed -E 's/<div id="footer">Generated on [[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{4}<\/div>//g' \
      $file > "${file}.tmp" && mv "${file}.tmp" $file
done

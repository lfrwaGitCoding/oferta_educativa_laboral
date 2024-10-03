#!/bin/bash
# Original script: https://gist.github.com/togume/83b4bf40e1528742374bbce338270f34
# From discussion: https://stackoverflow.com/questions/5722544/how-can-i-convert-an-mdb-access-file-to-mysql-or-plain-sql-file#5722674
# https://gist.github.com/jerryq27/73ee7391dfbab7aefb3b873cc8354b2e
# Changes from original:
# Table names with spaces
# Values with the ` or ' character
# Values with back slashes
# Large Text values from Access being limited to (255) characters


# TO DO:
# Add bash best practice, stopping on error, trace, etc.

TABLES=$(mdb-tables -1 "$1")

IFS=$'\n' # Internal Field Separator: for loops use spaces to separate input by default, this changes that default value to new lines.
for t in $TABLES
do
    # Surround the names in backticks so the spaces are ignored.
    echo "DROP TABLE IF EXISTS \`$t\`;"
done

# Increase the size of text fields by removing (255).
mdb-schema $1 mysql | sed -e 's/text (.[0-9]*)/text/g' 

for t in $TABLES
do
    # Escape ' by doubling '' and ` by doubling ``, 3rd regex removes any backslashes within quotes.
    mdb-export -D '%Y-%m-%d %H:%M:%S' -I mysql $1 $t | sed -e "s/'/''/g" -e 's/"\([0-9]*\)`\([0-9]*\)"/"\1``\2"/g; s/"\([A-Z]*\)\\"/"\1"/g'
done

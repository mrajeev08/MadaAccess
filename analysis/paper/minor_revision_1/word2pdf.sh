#!/bin/bash

cd analysis/paper/minor_revision_1

find . -type f \( -iname \*.doc -o -iname \*.docx \) -print0 | while read -d $'\0' file
 do
     automator -i "$file" ~/Desktop/w2pdf.app
 done

#!/bin/bash

cd analysis/paper/subfiles

find . -type f \( -iname \*.doc -o -iname \*.docx \) -print0 | while read -d $'\0' file
 do
     automator -i "$file" ~/Desktop/w2pdf.app
 done

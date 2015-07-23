#!/bin/sh
## Example of a broken script. Hit the Down Arrow button to ShellCheck it!
for f in $(ls *.m3u)
do
  grep -qi hq.*mp3 $f \
    && echo -e 'Playlist $f contains a HQ file in mp3 format'
done

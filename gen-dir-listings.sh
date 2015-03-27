#!/bin/bash
set -e
set -u

# This script recursively generates index.html files with directory listings
# for all (non-hidden) directories in CWD that do not contain an index.html
# file already (or the existing index.html file is a dir listing already).

# Borrowed and adopted from Christian Kaltepoth's script:
# http://blog.kaltepoth.de/posts/2010/09/06/github-maven-repositories.html

DIR_LISTING_HEADER="<!--DIR_LISTING-->"

OIFS="$IFS"
IFS=$'\n'
for DIR in $(find -P . -type d -not -path '*/\.*'); do
  echo "${DIR}"
  INDEX_FILE="${DIR}/index.html"
  if [[ ! -e ${INDEX_FILE} || $(head -1 ${INDEX_FILE}) == ${DIR_LISTING_HEADER} ]]; then
      (
        echo -e ${DIR_LISTING_HEADER}
        echo -e "<html>\n<body>\n<h1>Directory listing</h1>\n<hr/>\n<pre>"
        ls -1p "${DIR}" | grep -v "^\./$" \
            | grep -v "^index\.html$" \
            | awk '{ printf "<a href=\"%s\">%s</a>\n",$1,$1 }'
        echo -e "</pre>\n</body>\n</html>"
      ) > ${INDEX_FILE}
 fi
done
IFS="$OIFS"

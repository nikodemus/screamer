#!/bin/sh

set -e

BASE=`dirname $0```

$BASE/htmlize.sh/htmlize.sh $1 $2.tmp

cat > $2 <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html>
  <head>
EOF

echo "    <title>"`basename $1`"</title>" >> $2

cat >> $2 <<EOF
    <link rel="stylesheet" href="style.css" type="text/css">
  </head>
  <body>
EOF

printf "    <pre>" >> $2

cat $2.tmp >> $2

cat >> $2 <<EOF
</pre>
  </body>
</html>
EOF

rm $2.tmp

echo "$2 built"

#!/bin/sh

set -e

BASE=`dirname $0```

$BASE/htmlize.sh/htmlize.sh $1 $2.body

cat > $2 <<EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html>
  <head>
    <title>$3</title>
    <link rel="stylesheet" href="style.css" type="text/css">
EOF

cat $BASE/analytics.script >> $2

cat >> $2 <<EOF
  </head>
  <body>
    <pre>
EOF

cat $2.body >> $2

cat >> $2 <<EOF
    </pre>
  </body>
</html>
EOF

rm $2.body

echo "$2 built"

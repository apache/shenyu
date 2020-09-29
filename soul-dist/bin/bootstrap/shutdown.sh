PRGDIR=`dirname "$0"`
APP=$PRGDIR/../lib/soul-bootstrap.jar
BASE_DIR=$PRGDIR/..
PID_FILE=$BASE_DIR/logs/soul-bootstrap.pid

PID=$(cat $PID_FILE 2>/dev/null)
kill -KILL $PID 2>/dev/null
echo "Stopping $APP OK"

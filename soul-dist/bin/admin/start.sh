# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

noJavaHome=false
if [ -z "$JAVA_HOME" ] ; then
    noJavaHome=true
fi

if [ ! -e "$JAVA_HOME/bin/java" ] ; then
    noJavaHome=true
fi

if $noJavaHome ; then
    echo
    echo "Error: JAVA_HOME environment variable is not set."
    echo
    exit 1
fi

PRGDIR=`dirname "$0"`
APP=$PRGDIR/../lib/soul-admin.jar
BASE_DIR=$PRGDIR/..
APP_DIR=$BASE_DIR
LOG_DIR=$BASE_DIR/logs
PID_FILE=$LOG_DIR/soul-admin.pid

#cd $APP_DIR
#设置java的CLASSPATH

#==============================================================================

#set JAVA_OPTS
#JAVA_OPTS="-server -Xmx2048m  -Xms2048g -Xmn1024m  -Xss512k -XX:PermSize=128m"
JAVA_OPTS="-server -Xmx2048m  -Xms2048m -Xmn1024m  -Xss512k"
JAVA_OPTS="$JAVA_OPTS -XX:+AggressiveOpts"
JAVA_OPTS="$JAVA_OPTS -XX:+UseBiasedLocking"
JAVA_OPTS="$JAVA_OPTS -XX:+UseFastAccessorMethods"
JAVA_OPTS="$JAVA_OPTS -XX:+DisableExplicitGC"
JAVA_OPTS="$JAVA_OPTS -XX:+UseParNewGC"
JAVA_OPTS="$JAVA_OPTS -XX:ParallelGCThreads=20"
JAVA_OPTS="$JAVA_OPTS -XX:+UseConcMarkSweepGC"
JAVA_OPTS="$JAVA_OPTS -XX:+HeapDumpOnOutOfMemoryError"
JAVA_OPTS="$JAVA_OPTS -XX:MaxGCPauseMillis=850"
#JAVA_OPTS="$JAVA_OPTS -XX:+PrintGCDetail"
JAVA_OPTS="$JAVA_OPTS -XX:+CMSParallelRemarkEnabled"
#JAVA_OPTS="$JAVA_OPTS -XX:+UseCMSCompactAtFullCollection"
JAVA_OPTS="$JAVA_OPTS -XX:+UseCMSInitiatingOccupancyOnly"
JAVA_OPTS="$JAVA_OPTS -XX:CMSInitiatingOccupancyFraction=75"
JAVA_OPTS="$JAVA_OPTS -Xloggc:$LOG_DIR/gc.log"

mkdir -p $LOG_DIR
run_cmd="java $JAVA_OPTS -jar $APP --spring.config.location=file:$BASE_DIR/conf/"
$run_cmd >> $LOG_DIR/stdout.log 2>&1 &
echo $! > "$PID_FILE"
if [ $? -gt 0 ]; then
  echo "Starting $APP ERROR"
fi
  echo "Starting $APP OK"


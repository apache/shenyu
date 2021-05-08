@REM
@REM  Licensed to the Apache Software Foundation (ASF) under one or more
@REM  contributor license agreements.  See the NOTICE file distributed with
@REM  this work for additional information regarding copyright ownership.
@REM  The ASF licenses this file to You under the Apache License, Version 2.0
@REM  (the "License"); you may not use this file except in compliance with
@REM  the License.  You may obtain a copy of the License at
@REM
@REM      http://www.apache.org/licenses/LICENSE-2.0
@REM
@REM  Unless required by applicable law or agreed to in writing, software
@REM  distributed under the License is distributed on an "AS IS" BASIS,
@REM  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@REM  See the License for the specific language governing permissions and
@REM  limitations under the License.

@echo off

setlocal
set PRGDIR=%~dp0%
set BASE_DIR=%PRGDIR%..
set APP=%BASE_DIR%\lib\shenyu-bootstrap.jar
set LOG_DIR=%BASE_DIR%\logs

if not defined JAVA_HOME (
 echo "Error: JAVA_HOME environment variable is not set."
 exit
)

set JAVA_OPTS=-server -Xmx2048m  -Xms2048m -Xmn1024m  -Xss512k
::set JAVA_OPTS=%JAVA_OPTS% -XX:+AggressiveOpts
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseBiasedLocking
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseFastAccessorMethods
::set JAVA_OPTS=%JAVA_OPTS% -XX:+DisableExplicitGC
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseParNewGC
::set JAVA_OPTS=%JAVA_OPTS% -XX:ParallelGCThreads=20
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseConcMarkSweepGC
::set JAVA_OPTS=%JAVA_OPTS% -XX:+HeapDumpOnOutOfMemoryError
::set JAVA_OPTS=%JAVA_OPTS% -XX:MaxGCPauseMillis=850
::set JAVA_OPTS=%JAVA_OPTS% -XX:+PrintGCDetail
::set JAVA_OPTS=%JAVA_OPTS% -XX:+CMSParallelRemarkEnabled
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseCMSCompactAtFullCollection
::set JAVA_OPTS=%JAVA_OPTS% -XX:+UseCMSInitiatingOccupancyOnly
::set JAVA_OPTS=%JAVA_OPTS% -XX:CMSInitiatingOccupancyFraction=75
set JAVA_OPTS=%JAVA_OPTS% -Xloggc:%LOG_DIR%\gc.log

if not exist %LOG_DIR% ( mkdir %LOG_DIR% )
java %JAVA_OPTS% -jar %APP% --spring.config.location=file:%BASE_DIR%\conf\ >> "%LOG_DIR%\stdout.log"

endlocal

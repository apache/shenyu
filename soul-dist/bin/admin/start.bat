@echo off

setlocal
set PRGDIR=%~dp0%
set APP=%PRGDIR%\..\lib\soul-admin.jar
set BASE_DIR=%PRGDIR%\..
set APP_DIR=%BASE_DIR%
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
java %JAVA_OPTS% -jar %APP% -Dspring.config.location=file:%BASE_DIR%\conf\ >> "%LOG_DIR%\stdout.log"

endlocal

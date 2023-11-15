mkdir -p /tmp/shenyu-e2e/oracle
mkdir -p /tmp/shenyu-e2e/driver/oracle


wget -O /tmp/shenyu-e2e/oracle/ojdbc8.jar \
https://download.oracle.com/otn-pub/otn_software/jdbc/ojdbc8.jar || \
  wget -O /tmp/shenyu-e2e/oracle/ojdbc8.jar \
  https://download.oracle.com/otn-pub/otn_software/jdbc/ojdbc8.jar

cp db/init/oracle/schema.sql /tmp/shenyu-e2e/oracle/schema.sql

# How to start the integrated tests

There are two key points:
1. Using docker compose to build a test environment. We start MySQL, database, redis and other dependencies in docker;
2. Writing integrated tests under `shenyu-integrated-tests/src/test`.

To support the integrated tests, we must prepare some data. So we update `shenyu-admin` `schema.sql` file.

## Quick start
You must install Docker!

1. Build `dist` from source, run this command under root directory:
```shell
./mvnw clean package -Pdist,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
```
2. Build the examples docker,
```shell
cd shenyu-examples/shenyu-examples-http
docker build -t shenyu-test-http ./
```
3. Start the docker compose:
```shell
cd shenyu-integrated-test
docker compose up
```

4. Run the integrated test
```shell
./mvnw test
```
# How to start the integrated tests

There are two key points:
1. Using docker compose to build a test environment. We start MySQL, database, redis and other dependencies in docker;
2. Writing integrated tests under `shenyu-integrated-tests/src/test`.

To support the integrated tests, we must prepare some data. So we update `shenyu-admin` `schema.sql` file.

## Quick start
**You must install Docker**!

> If you are using windows, please notice that `-` is an invalid operator for terminals

Run those steps under this project's root directory 

1. Build `dist` from source, run this command under root directory:
```shell
./mvnw -B clean install -Prelease,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
```
2. Build the examples' docker, here is the `http` plugin example:
```shell
./mvnw -B clean install -DskipTests -f ./shenyu-examples/shenyu-examples-http/pom.xml
```
3. Build integrated gateway:
```shell
./mvnw -B clean install -DskipTests -f ./shenyu-integrated-test/shenyu-integrated-test-http/pom.xml
```
4. Start the docker compose:
```shell
docker-compose -f ./shenyu-integrated-test/shenyu-integrated-test-http/docker-compose.yml up -d
```

> On mac, you could `docker compose`, but if you are using ubuntu, you'd better using `docker-compose`

5. Run the integrated test
```shell
./mvnw test -f ./shenyu-integrated-test/shenyu-integrated-test-http/pom.xml
```

## How to write a new integrated tests?

### Add test cases for existing plugin's integrated tests

That's the easy case!

You just need to do those steps:
1. Prepare data. It means that you must insert some data into `shenyu-admin` database. 
   So, you need to update `shenyu-integated-test-{plugin}/admin-config/schema.sql`
   
2. (Optional) Update gateway configure. The configure file is under 
   `shenyu-integated-test-{plugin}/src/main/resources/application-local.yml`
   
3. Writing new test cases under  `shenyu-integated-test-{plugin}/src/test`

### Writing integrated tests for new plugin
In this case, we didn't write tests for this plugin, so you need to setup the environment by yourself.

#### Create a new module under `shenyu-integrated-test`

The name should be `shenyu-integrated-test-{plugin}`. 

You should read the shenyu's documents and add dependencies to `pom.xml` file.

Write `Dockerfile` for this module. We will use this module's image to star up docker compose. 
In this step you could copy other plugins' `Dockerfile` and update it.

#### Write Dockerfile for plugin's example module

We are using `shenyu-examples` module as the backend behind gateway. So you need to write `Dockerfile` for your plugin's example module.

#### Writing docker compose file

You could copy other plugins' `docker-compose` file. Update the image of example's image and gateway's image

#### Prepare data for integrated tests

Update `admin-config/schema.sql` to prepare data

#### Writing tests

Writing tests under `shenyu-integated-test-{plugin}/src/test`. You must run test locally before you commit your changes.

#### Writing new github workflow

You could copy other plugins' YAML file and then update the plugin name. And then you could make PR for us.
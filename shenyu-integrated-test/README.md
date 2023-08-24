# How to start the integrated tests

There are two key points:
1. Using docker compose to build a test environment. We start MySQL, database, redis and other dependencies in docker;
2. Write integrated tests under `shenyu-integrated-tests/src/test`.

## Preparation
1. Install and start docker and docker-compose.

## Quick start
> If you are using windows, please notice that `-` is an invalid operator for terminals.

Run those steps under this project's root directory 

1. Build `dist` from source, run this command under root directory:
```shell
./mvnw -B clean install -Prelease,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
```
2. Build integrated tests:
```shell
./mvnw -B clean install -Pit -DskipTests -f ./shenyu-integrated-test/pom.xml
```
4. Start the docker compose:
```shell
docker-compose -f ./shenyu-integrated-test/${{ matrix.case }}/docker-compose.yml up -d
```
> You need to replace ${{ matrix.case }} with the exact directory, such as shenyu-integrated-test-http.
> On mac, you could `docker compose`, but if you are using ubuntu, you'd better using `docker-compose`.
5. Run the integrated test:
```shell
./mvnw test -Pit -f ./shenyu-integrated-test/${{ matrix.case }}/pom.xml
```

## How to write a new integrated tests?

### Add test cases for existing plugin's integrated tests

That's the easy case!

You just need to do those steps:
1. (Optional) Update gateway configure. The configure file is under 
   `shenyu-integated-test-{plugin}/src/main/resources/application-local.yml`
   
2. Writing new test cases under  `shenyu-integated-test-{plugin}/src/test`.

### Write integrated tests for new plugin
In this case, we didn't write tests for this plugin, so you need to setup the environment by yourself.

#### Create a new module under `shenyu-integrated-test`

The name should be `shenyu-integrated-test-{plugin}`. 

You should read the shenyu's documents and add dependencies to `pom.xml` file.

Write `Dockerfile` for this module. We will use this module's image to star up docker compose. 
In this step you could copy other plugins' `Dockerfile` and update it.

#### Write Dockerfile for plugin's example module

We are using `shenyu-examples` module as the backend behind gateway. So you need to write `Dockerfile` for your plugin's example module.

#### Write docker compose file

You could copy other plugins' `docker-compose` file. Update the image of example's image and gateway's image

#### Prepare data for integrated tests

Now, we can request related path in `PluginController` in `shenyu-web` to modify the configuration of shenyu gateway.
You could extends `AbstractPluginDataInit` and call method in `AbstractPluginDataInit` to change the configuration in shenyu.

However, pls remember to clean after class.

#### Write tests

Writing tests under `shenyu-integated-test-{plugin}/src/test`. You must run test locally before you commit your changes.

#### Write new github workflow

You could copy other plugins' YAML file and then update the plugin name. And then you could make PR for us.

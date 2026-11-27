# How to start the integrated tests

Docker Compose starts the gateway, Admin, and other services required by each test case.
Test sources live under each case module's `src/test` directory.

## Preparation

Install JDK 17, Docker with the Compose plugin, and `curl`. Start Docker before building the images.

## Quick start

Run these commands from the repository root in Bash. This example uses the HTTP integration case.

1. Build and install the ShenYu distributions and images.

   ```bash
   ./mvnw -B clean install -Prelease,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
   ```

2. Build the HTTP example image and the integration gateway image.

   ```bash
   ./mvnw -B -f shenyu-examples/pom.xml -pl :shenyu-examples-http -am \
     clean install -Pexample -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
   ./mvnw -B -f shenyu-integrated-test/pom.xml -pl :shenyu-integrated-test-http -am \
     clean install -Pit -Dmaven.test.skip=true
   ```

3. Start the test stack and wait for the services to respond.

   ```bash
   case_dir=shenyu-integrated-test/shenyu-integrated-test-http
   docker compose -f "$case_dir/docker-compose.yml" up -d
   bash "$case_dir/script/healthcheck.sh"
   ```

4. Run the HTTP integration tests from the same shell.

   ```bash
   ./mvnw -B -f "$case_dir/pom.xml" test -Pit -DfailIfNoTests=true
   ```

5. Stop the test stack when finished.

   ```bash
   docker compose -f "$case_dir/docker-compose.yml" down
   ```

For another Compose-based case, select its Maven module, example images, and case directory.
The [integration test workflow](../.github/workflows/integrated-test.yml) shows how CI selects and builds the examples required by each case.

## How to write a new integrated tests?

### Add test cases for existing plugin's integrated tests

That's the easy case!

You just need to do those steps:
1. Update the gateway configuration in the case module's `src/main/resources` directory if needed.
2. Add test cases under `shenyu-integrated-test-{plugin}/src/test`.

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

Write tests under `shenyu-integrated-test-{plugin}/src/test` and run them locally before committing.

#### Write new github workflow

You could copy other plugins' YAML file and then update the plugin name. And then you could make PR for us.

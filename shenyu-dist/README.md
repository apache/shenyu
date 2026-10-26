## ShenYu Distribution

The `shenyu-dist` module is used to build packages and releases.

### Release compatibility checklist

Complete this checklist for every main repository or `shenyu-client-java` release. Record results in the [Java client and server compatibility matrix](../docs/java-client-compatibility.md#updating-the-matrix). The [#7602 migration guide](../shenyu-register-center/CLIENT_MIGRATION.md) defines Java 17 / Spring Boot 3 as the target, keeps gateway beat modules in main and requires one-way client-to-main dependencies. Historical Java 8 results and draft preparation results are separate from published support guarantees.

- [ ] Record exact client artifact versions and source tag/commit, Admin and bootstrap versions, application framework, registration transport, and sync channel.
- [ ] Confirm client and server JDK baselines from the release POMs and resolved dependencies; record build and runtime JDK vendors and versions separately.
- [ ] Run migration-client tests on Java 17 and verify Java 21 under main's CI policy. Use Java 8 only for an explicitly selected historical client fixture; test 23/25 only when claimed. Keep configured CI, reported local results, passing CI and released-pair evidence separate.
- [ ] Run server checks and Admin/bootstrap startup and routing smoke tests on every claimed server JDK. A client test on JDK 8 does not qualify the server for JDK 8.
- [ ] Test the old client with the candidate Admin and the candidate client with the oldest claimed Admin. Verify authentication, persisted metadata/URIs, heartbeat, offline, discovery, gateway sync and routing, plus API docs/MCP when claimed.
- [ ] Replay registration contract fixtures and verify the selected registry transport. Record protocol/capability changes, rolling upgrade behavior, and rollback results.
- [ ] For the client migration, release main-owned protocol/foundations before external client artifacts; remove staged main sources last. Confirm single-owner publication of every coordinate, retention of beat/registry modules in main and no independent-client dependency in main-owned code/tests/bootstrap/retained examples.
- [ ] Add or update exact version rows with dated, passing test evidence and supported, best-effort, or unsupported status. Preserve historical evidence and document remaining limitations.
- [ ] Link the updated matrix and any incompatible changes from the release notes before publishing the release.

### Quickstart

* git clone https://github.com/apache/shenyu.git
* mvn clean package -Dmaven.javadoc.skip=true -Dmaven.test.skip=true

Then you will see the `dist` directory in root directory.

### How to run docker
* build image and startup docker container

docker build
```
cd shenyu 
mvn clean package -Prelease,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
```

If build success, run `docker images` and you will see like this.
```
REPOSITORY                TAG                    IMAGE ID            CREATED             SIZE
apache/shenyu-bootstrap   2.2.1                  0002944cca64        About an hour ago   200MB
apache/shenyu-admin       2.2.1                  1a4b73d6dd6d        About an hour ago   215MB
```

run docker
```
docker network create shenyu
docker run -d -p 9095:9095 --net shenyu apache/shenyu-admin:2.2.1
docker run -d -p 9195:9195 --net shenyu apache/shenyu-bootstrap:2.2.1
```

If you want to override environment variables, you can do like this.

```
docker run -e "SPRING_PROFILES_ACTIVE=mysql" -e "spring.datasource.url=jdbc:mysql://192.168.1.9:3306/shenyu?useUnicode=true&characterEncoding=utf-8&useSSL=false" -e "spring.datasource.password=123456" -d -p 9095:9095 --net shenyu apache/shenyu-admin:2.2.1
```

Another way, bind volume and mounts

Put your `application.yml` in xxx directory, then run like this.

`docker run -v D:\tmp\conf:/opt/shenyu-admin/conf/ -d -p 9095:9095 --net shenyu apache/shenyu-admin:2.2.1`

* Finally. Go to http://IP:9095/ and login with `admin/123456`, enjoy it ^_^

_Note_: The Docker environment uses an H2 database by default.



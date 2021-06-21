## ShenYu Distribution

The `shenyu-dist` module is used to build packages and releases.

### Quickstart

* git clone https://github.com/apache/shenyu.git
* mvn clean package -Dmaven.javadoc.skip=true -Dmaven.test.skip=true

Then you will see the `dist` directory in root directory.

### How to run docker
* build image and startup docker container

docker build
```
cd shenyu 
mvn clean package -Pdist,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
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



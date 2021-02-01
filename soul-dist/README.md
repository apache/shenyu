## Soul Distribution

The `soul-dist` module is used to build packages and releases.

### Quickstart

* git clone https://github.com/dromara/soul.git
* mvn clean package -Dmaven.javadoc.skip=true -Dmaven.test.skip=true

Then you will see the `dist` directory in root directory.

### How to run docker
* build image and startup docker container

docker build
```
cd soul 
mvn clean package -Pdist,docker -Dmaven.javadoc.skip=true -Dmaven.test.skip=true
```

If build success, run `docker images` and you will see like this.
```
REPOSITORY            TAG                    IMAGE ID            CREATED             SIZE
soul/soul-bootstrap   2.2.1                  0002944cca64        About an hour ago   200MB
soul/soul-admin       2.2.1                  1a4b73d6dd6d        About an hour ago   215MB
```

run docker
```
docker network create soul
docker run -d -p 9095:9095 --net soul soul/soul-admin:2.2.1
docker run -d -p 9195:9195 --net soul soul/soul-bootstrap:2.2.1
```

* Finally. Go to http://IP:9095/ and login with `admin/123456`, enjoy it ^_^

_Note_: The Docker environment uses an H2 database by default.

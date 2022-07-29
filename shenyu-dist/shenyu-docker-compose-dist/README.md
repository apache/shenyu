### Run ShenYu By docker-compose

1. Execute the install-script.

```shell
$ curl -O https://raw.githubusercontent.com/apache/shenyu/master/shenyu-dist/shenyu-docker-compose-dist/src/main/resources/install.sh

$ sh ./install.sh ${version} #please replace the ${version}, such as v2.4.2 or latest
```

2. Modify the configuration file.

3. Start docker-compose

```shell
docker-compose -f ./docker-compose.yaml up -d
```

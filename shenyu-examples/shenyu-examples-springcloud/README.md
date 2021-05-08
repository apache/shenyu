# Soul example SpringCloud
***
## Run
first you should run eureka: soul-example-eureka

Then you can run server with http/zookeeper/etcd register center

### run with etcd register center
#### Soul-Admin config
add etcd register center dependency in soul-admin:

```xml
        <dependency>
            <groupId>org.apache.shenyu</groupId>
            <artifactId>soul-register-server-etcd</artifactId>
            <version>${project.version}</version>
        </dependency>
```

set etcd register config in soul-admin:

```yaml
soul:
  register:
    registerType: etcd
    serverLists : http://localhost:2379
```

#### soul-examples-springcloud config
set etcd register config:

```yaml
soul:
  client:
    registerType: etcd
    serverLists: http://localhost:2379
    props:
      contextPath: /springcloud
```

run SoulTestSpringCloudApplication

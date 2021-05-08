# # Soul example Sofa
***
## Run
You can run server with http/zookeeper/etcd register center

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

#### soul-examples-sofa-service config
set etcd register config:

```yaml
soul:
  client:
    registerType: etcd #http #zookeeper #etcd
    serverLists: http://localhost:2379 #http://localhost:9095 #localhost:2181 #http://localhost:2379
    props:
      contextPath: /sofa
      appName: sofa
      port: 8888
```

run TestSofaApplication

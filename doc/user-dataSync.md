---
title: 使用不同的数据同步策略
keywords: soul
description: 使用不同的数据同步策略
---

## 说明

* 数据同步是指将 `soul-admin` 配置的数据，同步到 `soul` 集群中的JVM内存里面，是网关高性能的关键。

* 实现原理，请看： [数据同步](dataSync.md)。

* 文中所说的网关，是指你搭建的网关环境，请看：[搭建环境](setup.md)。

## websocket同步（默认方式，推荐）

* 网关配置（记得重启）
  
    * 首先在 `pom.xml` 文件中 引入以下依赖：
 ```xml
    <!--soul data sync start use websocket-->
    <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-sync-data-websocket</artifactId>
      <version>2.2.0</version>
    </dependency>
   ``` 
   * 在 springboot的 yml 文件中进行如下配置:
  ```yaml
  soul :
      sync:
          websocket :
               urls: ws://localhost:9095/websocket
  #urls:是指 soul-admin的地址，如果有多个，请使用（,）分割.
   ```

* soul-admin 配置, 默认是开启 websocket 同步的，如果您想关闭，请指定`soul.sync.websocket.enabled=false`

* 当建立连接以后会全量获取一次数据，以后的数据都是增量的更新与新增，性能好。

* 支持断线重连 （默认30秒）。


## zookeeper同步

* 网关配置（记得重启）
  
    * 首先在 `pom.xml` 文件中 引入以下依赖：

 ```xml
    <!--soul data sync start use zookeeper-->
      <dependency>
           <groupId>org.dromara</groupId>
            <artifactId>soul-spring-boot-starter-sync-data-zookeeper</artifactId>
            <version>2.2.0</version>
      </dependency>
 ```
  
   * 在 springboot的 yml 文件中进行如下配置:
 ```yaml
  soul :
      sync:
          zookeeper:
               url: localhost:2181
               sessionTimeout: 5000
               connectionTimeout: 2000
  #url: 配置成你的zk地址，集群环境请使用（,）分隔
 ```

* soul-admin 配置, 或在 soul-admin 启动参数中设置 `--soul.sync.zookeeper.url='你的地址' `,然后重启服务。
```yaml
soul:
  sync:
    zookeeper:
        url: localhost:2181
        sessionTimeout: 5000
        connectionTimeout: 2000
```
* 使用zookeeper同步机制也是非常好的,时效性也高，我们生产环境使用的就是这个，但是也要处理zk环境不稳定，集群脑裂等问题.

## http长轮询同步

* 网关配置（记得重启）
  
    * 首先在 `pom.xml` 文件中 引入以下依赖：

 ```xml
    <!--soul data sync start use zookeeper-->
      <dependency>
           <groupId>org.dromara</groupId>
            <artifactId>soul-spring-boot-starter-sync-data-http</artifactId>
            <version>2.2.0</version>
      </dependency>
   ```
  
   * 在 springboot的 yml 文件中进行如下配置:
   ```yaml
  soul :
      sync:
          http:
               url: http://localhost:9095
  #url: 配置成你的 soul-admin的 ip与端口地址，多个admin集群环境请使用（,）分隔。
   ```
* soul-admin 配置, 默认是开启 http 同步的，如果您想关闭，请指定`soul.sync.http.enabled=false`
```yaml
soul:
  sync:
     http:
       refresh-interval: 5m # 默认5min刷新一次本地缓存
       enabled: true # 默认启用http同步策略
```

* http长轮询使得网关很轻量，时效性略低。 

* 其根据分组key来拉取，如果数据量过大，过多，会有一定的影响。 什么意思呢？就是一个组下面的一个小地方更改，会拉取整个的组数据。

## nacos同步

* 网关配置（记得重启）
  
    * 首先在 `pom.xml` 文件中 引入以下依赖：
 ```xml
    <!--soul data sync start use zookeeper-->
      <dependency>
           <groupId>org.dromara</groupId>
            <artifactId>soul-spring-boot-starter-sync-data-nacos</artifactId>
            <version>2.2.0</version>
      </dependency>
   ```
  
  * 在 springboot的 yml 文件中进行如下配置:
 ```yaml
  soul :
      sync:
         nacos:
              url: localhost:8848
              namespace: 1c10d748-af86-43b9-8265-75f487d20c6c
              acm:
                enabled: false
                endpoint: acm.aliyun.com
                namespace: 
                accessKey: 
                secretKey: 
  #url: 配置成你的nacos地址，集群环境请使用（,）分隔。
  # 其他参数配置，请参考naocs官网。
 ```
* soul-admin 配置, 或在 soul-admin 启动参数中使用 `--` 的方式一个一个传值。
```yaml
soul :
      sync:
         nacos:
              url: localhost:8848
              namespace: 1c10d748-af86-43b9-8265-75f487d20c6c
              acm:
                enabled: false
                endpoint: acm.aliyun.com
                namespace: 
                accessKey: 
                secretKey: 
```

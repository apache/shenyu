---
title: 环境搭建
keywords: soul
description: 环境搭建
---

## 说明

* soul 2.2.0以后都是基于插件化可插拔的思想，本文是说明,如何基于soul搭建属于你自己网关。

* 请确保你的机器安装了JDK 1.8+ , Mysql 5.0 + 。

## 启动 `Soul-Admin`

* 下载`soul-admin.jar`包，并启动.
```yaml
> wget  https://yu199195.github.io/jar/soul-admin.jar

> java -jar soul-admin.jar --spring.datasource.url="jdbc:mysql://你的url:3306/soul?useUnicode=true&characterEncoding=utf-8&useSSL=false"  
  --spring.datasource.username='you username'  --spring.datasource.password='you password'
 
```
* 访问 `http://localhost:9095/index.html ` 默认的用户名： admin  密码:123456。

## 搭建自己的网关（推荐）

* 首先你新建一个空的springboot项目，可以参考 soul-bootstrap. 也可以在spring官网:[https://spring.io/quickstart]

* 引入如下jar包：
```xml
  <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-webflux</artifactId>
        <version>2.2.2-RELEASE</version>
  </dependency>

  <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-actuator</artifactId>
        <version>2.2.2-RELEASE</version>
  </dependency>

  <!--soul gateway start-->
  <dependency>
        <groupId>org.dromara</groupId>
        <artifactId>soul-spring-boot-starter-gateway</artifactId>
        <version>2.2.0</version>
  </dependency>
  
   <!--soul data sync start use websocket-->
   <dependency>
        <groupId>org.dromara</groupId>
        <artifactId>soul-spring-boot-starter-sync-data-websocket</artifactId>
        <version>2.2.0</version>
   </dependency>
```

* 在你的 `application.yaml` 文件中加上如下配置：
```yaml
spring:
   main:
     allow-bean-definition-overriding: true

management:
  health:
    defaults:
      enabled: false
soul :
    sync:
        websocket :
             urls: ws://localhost:9095/websocket  //设置成你的soul-admin地址
swagger:
  enable: true
```
* 你的项目环境搭建完成,启动你的项目。











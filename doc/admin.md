---
title: 启动admin
keywords: admin
description: 启动admin
---


### 说明

 * soul-admin 使用了mysql数据库,启动前请确保你正确安装了mysql.

 * soul-admin 是一个springboot的jar包,启动方式,可以直接 `java -jar soul-admin.jar` ,当然如果您也可以根据脚本来指定相关的jvm参数.
 
 * soul-admin 会自动创建数据库，以及表结构，并初始化默认数据.

### 服务端启动

* 拉取jar包，并启动
```
> wget  https://yu199195.github.io/jar/soul-admin.jar

> java -jar soul-admin.jar --spring.datasource.url="jdbc:mysql://你的url:3306/soul?useUnicode=true&characterEncoding=utf-8&zeroDateTimeBehavior=CONVERT_TO_NULL&failOverReadOnly=false&autoReconnect=true&useSSL=false"  
  --spring.datasource.username='you username' --spring.datasource.password='you password' 
 
```
* 数据库无访问密码,将 **--spring.datasource.password='you password'** 去掉即可
```
> java -jar soul-admin.jar --spring.datasource.url="jdbc:mysql://你的url:3306/soul?useUnicode=true&characterEncoding=utf-8&zerodatetimebehavior=CONVERT_TO_NULL&failOverReadOnly=false&autoReconnect=true&useSSL=false"  
  --spring.datasource.username='you username' 
```
* 遇到如下错误,将 **zerodatetimebehavior=CONVERT_TO_NULL** 去掉即可
```java
    java.sql.SQLException: The connection property 'zeroDateTimeBehavior' only accepts values of the form: 'exception', 'round' or 'convertToNull'. The value 'CONVERT_TO_NULL' is not in this set.
```
* 访问 `http://localhost:9095/index.html ` 默认的用户名： admin  密码:123456

* 如果已经使用 soul, 并且未修改默认秘钥,明文密码 123456 对应的 密文是 `jHcpKkiDbbQh7W7hh8yQSA==`

### 本地启动
 

*  拉取代码
   ```
   > git clone https://github.com/Dromara/soul.git

   > cd soul

   > mvn -DskipTests clean install -U
   ```
   
 * 使用你的idea 打开项目.
 * 修改yml文件，修改你的数据库注意环境,默认使用 `application-local.yml`.
```yml

server:
  port: 9095
  address: 0.0.0.0

spring:
  thymeleaf:
    cache: true
    encoding: utf-8
    enabled: true
    prefix: classpath:/static/
    suffix: .html
  datasource:
    url: jdbc:mysql://你的地址:3306/soul-open?useUnicode=true&characterEncoding=utf-8&zeroDateTimeBehavior=CONVERT_TO_NULL&failOverReadOnly=false&autoReconnect=true&useSSL=false
    username: 你的用户名
    password: 你的密码
    dbcp2:
      driver-class-name: com.mysql.jdbc.Driver

mybatis:
  config-location: classpath:/mybatis/mybatis-config.xml
  mapper-locations: classpath:/mappers/*.xml

```

* 启动 `org.dromara.soul.admin.SoulAdminApplication`.

* 访问  http://localhost:9095/index.html  默认的用户名和密码为 admin 123456


 

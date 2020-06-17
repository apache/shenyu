---
title: divide插件
keywords: divide
description: divide插件
---

## 说明

* divide插件是网关处理 `http协议`请求的核心处理插件。

## 插件设置

* 开启插件, `soul-admin` --> 插件管理--> `divide` 设置为启用。

* divide插件，配合如下 starter一起才能生效，具体请看：[http用户](user-http.md)。

```xml
  <!--if you use http proxy start this-->
   <dependency>
       <groupId>org.dromara</groupId>
       <artifactId>soul-spring-boot-starter-plugin-divide</artifactId>
       <version>2.2.0</version>
   </dependency>

   <dependency>
       <groupId>org.dromara</groupId>
       <artifactId>soul-spring-boot-starter-plugin-httpclient</artifactId>
       <version>2.2.0</version>
   </dependency>

```

## 插件讲解

* divide插件是进行http正向代理的插件，所有http类型的请求，都是由该插件进行负载均衡的调用。

* 选择器和规则，请详细看 : [选择器规则](selector.md)。

* http配置，是网关匹配到流量以后，真实调用的http配置，可以配置多个，设置负载均衡权重，具体的负载均衡策略，在规则中指定。
  * 配置详解 ：

     * 第一个框：hostName，一般填写 `localhost`，该字段暂时没使用。
  
     * 第二个框：http协议，一般填写 `http://` 或者 `https://` ,不填写默认为:`http://`
  
     * 第三个框：ip与端口，这里填写你真实服务的 ip + 端口。
  
     * 第四个框：负载均衡权重。
     
  * ip + port 检测
  
     * 在soul-admin 会有一个定时任务来扫描 配置的ip端口，如果发现下线，则会除该 ip + port  
     
     * 可以进行如下配置 ：
     
```yaml
      soul.upstream.check:true  默认为 ture，设置为false，不检测
      soul.upstream.scheduledTime:10  定时检测时间间隔，默认10秒
 ```  
  
 
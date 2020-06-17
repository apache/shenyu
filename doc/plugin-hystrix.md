---
title: hystrix插件
keywords: soul
description: hystrix插件
---

## 说明

* hystrix插件是网关用来对流量进行熔断的核心实现。

* 使用信号量的方式来处理请求。


## 插件设置

* 在 `soul-admin` -->  插件管理 --> `hystrix` ,设置为开启。

* 如果用户不使用，则在 `soul-admin` 后台把此插件停用。


## 插件使用

* 在网关的 pom.xml 文件中添加 hystrix的支持。
```xml
  <!-- soul hystrix plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-hystrix</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul hystrix plugin end-->
``` 

* 选择器和规则，请详细看 : [选择器规则](selector.md)

* Hystrix处理详解：

    * 跳闸最小请求数量 ：最小的请求量，至少要达到这个量才会触发熔断
    
    * 错误半分比阀值 ： 这段时间内，发生异常的百分比。
    
    * 最大并发量 ： 最大的并发量
    
    * 跳闸休眠时间(ms) ：熔断以后恢复的时间。
    
    * 分组Key： 一般设置为:contextPath
    
    * 命令Key: 一般设置为具体的 路径接口。


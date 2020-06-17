---
title: rateLimiter插件
keywords: rateLimiter
description: rateLimiter插件
---

## 说明

* 限流插件，是网关对流量管控限制核心的实现。

* 可以到接口级别，也可以到参数级别，具体怎么用，还得看你对流量配置。


## 技术方案

* 采用redis令牌桶算法进行限流。

* 流程图：
  ![](https://yu199195.github.io/images/soul/limiting.png)

  
## 插件设置

* 在 `soul-admin`--> 插件管理--> `rate_limiter` 将其设置为开启。

* 在插件中，对redis进行配置。

* 目前支持redis的单机，哨兵，以及集群模式。

* 如果是哨兵，集群等多节点的，在URL中的配置，请对每个实列使用 `;` 分割. 如 192.168.1.1:6379;192.168.1.2:6379。

* 如果用户无需使用，在admin后台把插件禁用。 
 
## 插件使用

* 在网关的 pom.xml 文件中添加 rateLimiter的支持。

```xml
  <!-- soul ratelimiter plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-ratelimiter</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul ratelimiter plugin end-->
``` 

* 选择器和规则，请详细看 : [选择器规则](selector.md)。
  
* 速率：是你允许用户每秒执行多少请求，而丢弃任何请求。这是令牌桶的填充速率。
  
* 容量 ：是允许用户在一秒钟内执行的最大请求数。这是令牌桶可以保存的令牌数。
  

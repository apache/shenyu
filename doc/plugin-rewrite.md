---
title: rewrite插件
keywords: rewrite
description: rewrite插件
---

# 说明

* soul网关在对目标服务进行代理调用的时候，还容许用户使用 `rewrite` 插件来重写请求路径


## 插件设置

* 在 `soul-admin` --> 插件管理 --> `rewrite` ,设置为开启。

* 在网关的 pom.xml 文件中添加 `rewrite` 的支持。

* 如果用户不需要，可以把插件禁用。

```xml
  <!-- soul rewrite plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-rewrite</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul rewrite plugin end-->
``` 

* 选择器和规则，请详细看 : [选择器规则](selector.md)。

  * 只有匹配的请求，才会进行重写。

## 场景

* 顾名思义，重新插件就是对uri的重新定义。

* 当匹配到请求后，设置自定义的路径，那么自定义的路径就会覆盖之前的真实路径。

* 在调用的时候，就会使用用户自定义的路径。

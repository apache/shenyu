---
title: waf插件
keywords: waf
description: waf插件
---

## 说明

* waf插件，是网关的用来对流量实现防火墙功能的核心实现。

## 插件设置

* 在 `soul-admin` --> 插件管理-> `waf` 设置为开启。

* 如果用户不想使用此功能，请在admin后台停用此插件。

* 插件编辑里面新增配置模式。
```yaml
{"model":"black"}  
# 默认为黑名单模式，设置值为 mixed 则为混合模式，下面会专门进行讲解
```

## 插件使用

* 在网关的 pom.xml 文件中添加 `waf` 的支持。
```xml
  <!-- soul waf plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-waf</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul waf plugin end-->
``` 

* 选择器和规则，请详细看 : [选择器规则](selector.md)

  * 当 `module` 设置为 `black` 模式的时候, 只有匹配的流量才会执行拒绝策略，不匹配的，直接会跳过。
  
  * 当 `module` 设置为 `mixed` 模式的时候，所有的流量都会通过 waf插件，针对不同的匹配流量，用户可以设置是 拒绝，还是通过。

## 场景

* waf插件也是soul的前置插件，主要用来拦截非法请求，或者异常请求，并且给与相关的拒绝策略。

* 当你发现有大的攻击的适合，你可以根据ip或者host来进行匹配，拦截掉非法的ip与host，设置reject策略。

* 关于如何确定 ip 与 host 值，请看[ip与host](dev-iphost.md)
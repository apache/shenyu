---
title: dubbo插件
keywords: dubbo
description: dubbo插件
---

## 说明

* dubbo插件是将`http协议` 转换成`dubbo协议` 的插件,也是网关实现dubbo泛化调用的关键。

* dubbo插件需要配合元数据才能实现dubbo的调用，具体请看: [元数据](metaData.md)。

* apache dubbo 和 alibaba dubbo用户，都是使用该同一插件。


## 插件设置

* 在 `soul-admin` --> 插件管理-> `dubbo` 设置为开启。

* 在dubbo插件的配置中，配置如下: 配置dubbo的注册中心。
```yaml
{"register":"zookeeper://localhost:2181"} or {"register":"nacos://localhost:8848"} 
```
* 插件需要配合依赖 `starter` 进行使用,具体请看: [dubbo用户](user-dubbo.md)。

* 选择器和规则，请详细看 : [选择器规则](selector.md)。

## 元数据

* 每一个dubbo接口方法，都会对于一条元数据，可以在 soul-admin -->元数据管理，进行查看。

* 路径：就是你http请求的路径。 

* rpc扩展参数,对应为dubbo接口的一些配置，调整的化，请在这里修改，支持json格式,以下字段：

```yaml
{"timeout":10000,"group":"",version":"","loadbalance":"","retries":1,"url":""}
```



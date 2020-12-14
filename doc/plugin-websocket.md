---
title: websocket支持
keywords: soul
description: websocket支持
---

## 说明

* soul网关是支持 websocket的代理。

* websocket支持中，使用了divide插件。


## 插件设置

* 在 `soul-admin` --> 插件管理 --> ` divide `,设置为开启。

* 在网关的 pom.xml 文件中新增依赖
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
## 请求路径

* 使用soul代理websocket的时候，其请求路径为（列子）: `ws://localhost:9195/?module=ws&method=/websocket&rpcType=websocket`。

```yaml
参数详解:
1.localhost:8080 是soul启动的ip和端口。
2.module（必填）:值是你用来匹配selector的关键
3.method （参数）: 你的 websocket路径，同时也用做匹配rule
4.rpcType ：websocket 必填，且必须为websocket
```

* 在 `divide`插件中选择器新增一条配置 ，如下

![](https://yu199195.github.io/images/soul/websocket-selector.png)


* 在这一条选择器下新增一条 规则 ：

![](https://yu199195.github.io/images/soul/websocket-rule.png)


* 总结 ，这个时候注意看你的路径 `ws://localhost:9195/?module=ws&method=/websocket&rpcType=websocket`。

  它就会被你新增的选择器规则匹配，然后 代理的的真实websocket地址为 : `127.0.0.1:8080/websocket`,这样soul就进行的websocket的代理。
  
  你就可以进行和websocket服务进行通信了,就是这么简单。
  
* 最后再说一句，module，method 命名和值，你完全可以自己来决定，我的只是列子，只要选择器 和规则能够匹配就行。  
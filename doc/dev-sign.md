---
title: 自定义sign插件检验
keywords: soul
description: 自定义sign插件检验
---


## 说明

* 用户可以自定义签名认证算法来实现验证。

###  扩展

*  默认的实现为 `org.dromara.soul.plugin.sign.service.DefaultSignService`。

*  新增一个类 A 实现  `org.dromara.soul.plugin.api.SignService`。

```java
 public interface SignService {
 
     /**
      * Sign verify pair.
      *
      * @param exchange   the exchange
      * @return the pair
      */
     Pair<Boolean, String> signVerify(ServerWebExchange exchange);
 }

```

* Pair中返回true,表示验证通过，为false的时候，会把String中的信息输出到前端。

* 把你新增的实现类注册成为spring的bean,如下

```java
   @Bean
   public SignService a() {
         return new A
   }
```




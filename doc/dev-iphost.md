---
title: 正确获取Ip与host
keywords: soul
description: 正确获取Ip与host
---

## 说明

* 本文是说明，如果网关前面有一层`nginx` 的时候，如何获取正确的ip与端口。

* 获取正确的之后，在插件以及选择器中，可以根据 ip，与host来进行匹配。


##  默认实现

*  在soul网关自带实现为:`org.dromara.soul.web.forwarde.ForwardedRemoteAddressResolver`。

*  它需要你在 `nginx` 设置 `X-Forwarded-For`,以便来或者正确的 ip 与 host。


## 扩展实现

* 新增一个类A，实现`org.dromara.soul.plugin.api.RemoteAddressResolver`

```java
public interface RemoteAddressResolver {

    /**
     * Resolve inet socket address.
     *
     * @param exchange the exchange
     * @return the inet socket address
     */
    default InetSocketAddress resolve(ServerWebExchange exchange) {
        return exchange.getRequest().getRemoteAddress();
    }

}
```

* 把你新增的实现类注册成为spring的bean,如下

```java
   @Bean
   public SignService a() {
         return new A
   }
```






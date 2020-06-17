---
title: soul性能优化
keywords: soul
description: soul性能优化
---

## 说明

* 本文主要介绍如果对soul进行优化


## 本身消耗

* soul本身所有的操作，都是基于jvm内存来匹配，本身消耗时间大概在 1-3 ms左右。

## 底层netty调优

* soul内置依赖 `spring-webflux` 而其底层是使用的netty。这一块只要是使用的netty线程模型。

* 我们可以自定义netty的相关参数来对soul 进行优化,以下是示例：

```java
   @Bean
    public NettyReactiveWebServerFactory nettyReactiveWebServerFactory() {
        NettyReactiveWebServerFactory webServerFactory = new NettyReactiveWebServerFactory();
        webServerFactory.addServerCustomizers(new EventLoopNettyCustomizer());
        return webServerFactory;
    }

    private static class EventLoopNettyCustomizer implements NettyServerCustomizer {

        @Override
        public HttpServer apply(final HttpServer httpServer) {
            return httpServer
                    .tcpConfiguration(tcpServer -> tcpServer
                            .runOn(LoopResources.create("soul-netty", 1, DEFAULT_IO_WORKER_COUNT, true), false)
                            .selectorOption(ChannelOption.SO_REUSEADDR, true)
                            .selectorOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                            .option(ChannelOption.TCP_NODELAY, true)
                            .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT));
        }
    }
```

* 这个类在 soul-bootstrap中已经内置，在压测的时候，可以根据自己的需求来进行优化设置。

* 业务线程模型可以看[线程模型](dev-thread.md)









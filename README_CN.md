<p align="center" >
    <a href="https://dromara.org"><img src="https://shenyu.apache.org/img/logo/apache-shenyu.png" width="45%"></a>
</p>
<p align="center">
  <strong>应用于所有微服务场景的，可扩展、高性能、响应式的 API 网关解决方案</strong>
</p>
<p align="center">
  <a href="https://shenyu.apache.org/">https://shenyu.apache.org/</a>
</p>

<p align="center">
  <a href="https://github.com/apache/shenyu/blob/master/README.md">English</a> | 简体中文
</p>

<p align="center">
    <a target="_blank" href="https://search.maven.org/search?q=g:org.apache%20AND%20a:shenyu">
        <img src="https://img.shields.io/maven-central/v/org.apache/shenyu.svg?label=maven%20central" />
    </a>
    <a target="_blank" href="https://github.com/apache/shenyu/blob/master/LICENSE">
        <img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg?label=license" />
    </a>
    <a target="_blank" href="https://www.oracle.com/technetwork/java/javase/downloads/index.html">
        <img src="https://img.shields.io/badge/JDK-8+-green.svg" />
    </a>
    <a target="_blank" href="https://github.com/apache/shenyu/actions">
        <img src="https://github.com/apache/shenyu/workflows/ci/badge.svg" />
    </a>
   <a target="_blank" href='https://github.com/apache/shenyu'>
        <img src="https://img.shields.io/github/forks/apache/shenyu.svg" alt="github forks"/>
   </a>
   <a target="_blank" href='https://github.com/apache/shenyu'>
        <img src="https://img.shields.io/github/stars/apache/shenyu.svg" alt="github stars"/>
   </a>
   <a target="_blank" href='https://github.com/apache/shenyu'>
        <img src="https://img.shields.io/github/contributors/apache/shenyu.svg" alt="github contributors"/>
   </a>    
   <a target="_blank" href="https://codecov.io/gh/apache/incubator-shenyu">
        <img src="https://codecov.io/gh/apache/incubator-shenyu/branch/master/graph/badge.svg" />
   </a>
</p>
<br/>

--------------------------------------------------------------------------------

# 架构

 ![](https://shenyu.apache.org/img/architecture/shenyu-framework.png)  

--------------------------------------------------------------------------------

# 结构图

![](https://shenyu.apache.org/img/shenyu/activite/shenyu-xmind.png)

--------------------------------------------------------------------------------

# 模块

 * shenyu-admin : 插件和其他信息配置的管理后台

 * shenyu-bootstrap : 用于启动项目，用户可以参考

 * shenyu-client : 用户可以使用 Spring MVC，Dubbo，Spring Cloud 快速访问
 
 * shenyu-disruptor : 基于disruptor的封装
  
 * shenyu-register-center : shenyu-client提供各种rpc接入注册中心的支持
  
 * shenyu-common : 框架的通用类

 * shenyu-dist : 构建项目

 * shenyu-metrics : prometheus（普罗米修斯）实现的 metrics

 * shenyu-plugin : ShenYu 支持的插件集合

 * shenyu-spi : 定义 ShenYu spi

 * shenyu-spring-boot-starter : 支持 spring boot starter

 * shenyu-sync-data-center : 提供 ZooKeeper，HTTP，WebSocket，Nacos 的方式同步数据

 * shenyu-examples : RPC 示例项目

 * shenyu-web : 包括插件、请求路由和转发等的核心处理包

--------------------------------------------------------------------------------

# 功能特点

   * 提供了诸如限流、熔断、转发 、重写、重定向、和路由监控等插件；
   * 支持 HTTP、RESTFul、WebSocket、Dubbo、 GRPC、 Tars、 Spring Cloud 代理；
   * 支持热插拔，用户可以定制化开发；
   * 为了灵活的适配，选择器和规则可以动态的适配；
   * 支持集群部署；
   * 支持 A/B 测试和灰度发布。

--------------------------------------------------------------------------------

# 插件

无论请求何时进入，ShenYu 会通过响应链执行所有已打开的插件。

插件是 ShenYu 的灵魂，并且插件也是可扩展和热插拔的。

不同的插件实现不同的功能。

当然，用户也可以定制化插件去满足他们自己的需求。

如果你有定制化插件的需求，请参看这里：[custom-plugin](https://shenyu.apache.org/zh/projects/shenyu/custom-plugin/)

--------------------------------------------------------------------------------

# 选择器和规则

选择器和规则会根据 HTTP 的请求头来路由你的请求。

选择器是你的第一个路由，它是粗粒度的，举个例子，模块级别。

规则是你的第二个路由，即你认为你的请求应该做什么，举个例子，模块中的方法级别。

选择器和规则只匹配一次，然后返回匹配。因此，最粗粒度应排在最后。

--------------------------------------------------------------------------------

# 数据缓存 & 数据同步

所有的数据都被缓存在 JVM 的 ConcurrentHashMap 中，所以它非常快。

当用户在后台界面改变配置信息时，ShenYu 通过监听 ZooKeeper node，WebSocket push，HTTP longPull 来动态更新缓存。

  ![](https://shenyu.apache.org/img/shenyu/dataSync/shenyu-config-processor-zh.png)

  ![](https://shenyu.apache.org/img/shenyu/dataSync/config-strategy-processor-zh.png)

--------------------------------------------------------------------------------

# 必要条件

   * JDK 1.8+

--------------------------------------------------------------------------------

# 关于

ShenYu 已经被很多公司广泛使用在越来越多的业务系统，它能以高性能和灵活性让我们方便快捷的集成自己的服务和 API 。

在中国的双 11 购物狂欢节中，ShenYu集群成功支撑了海量的互联网业务。

--------------------------------------------------------------------------------

# 文档 & 网站

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://shenyu.apache.org/projects/shenyu/overview)
[![CN doc](https://img.shields.io/badge/document-Chinese-blue.svg)](https://shenyu.apache.org/zh/projects/shenyu/overview)

--------------------------------------------------------------------------------

# Github stars趋势

[![Stargazers over time](https://starchart.cc/apache/incubator-shenyu.svg)](https://starchart.cc/apache/incubator-shenyu.svg)

--------------------------------------------------------------------------------

# 目前已知用户

为了便于登记，欢迎已经使用了 ShenYu 的公司在 [https://github.com/apache/shenyu/issues/68](https://github.com/apache/shenyu/issues/68) 注册。（仅适用于开源用户）

所有用户 : [Known Users](https://shenyu.apache.org/awesome/)

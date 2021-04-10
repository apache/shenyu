<p align="center" >
    <a href="https://dromara.org"><img src="https://dromara.org/img/logo/soul.png" width="45%"></a>
</p>
<p align="center">
  <strong>应用于所有微服务场景的，可扩展、高性能、响应式的 API 网关解决方案</strong>
</p>
<p align="center">
  <a href="https://dromara.org">https://dromara.org/</a>
</p>

<p align="center">
  <a href="https://github.com/dromara/soul/blob/master/README.md">English</a> | 简体中文
</p>

<p align="center">
    <a target="_blank" href="http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.dromara%22%20AND%soul">
        <img src="https://img.shields.io/maven-central/v/org.dromara/soul.svg?label=maven%20central" />
    </a>
    <a target="_blank" href="https://github.com/Dromara/soul/blob/master/LICENSE">
        <img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg?label=license" />
    </a>
    <a target="_blank" href="https://www.oracle.com/technetwork/java/javase/downloads/index.html">
        <img src="https://img.shields.io/badge/JDK-8+-green.svg" />
    </a>
    <a target="_blank" href="https://github.com/dromara/soul">
        <img src="https://github.com/dromara/soul/workflows/ci/badge.svg" />
    </a>
    <a href="https://www.codacy.com/app/yu199195/soul?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=Dromara/soul&amp;utm_campaign=Badge_Grade">
        <img src="https://api.codacy.com/project/badge/Grade/4367ffad5b434b7e8078b3a68cc6398d"/>
    </a>
    <a target="_blank" href='https://gitee.com/dromara/soul/stargazers'>
        <img src='https://gitee.com/dromara/soul/badge/star.svg?theme=gvp' alt='gitee stars'/>
   </a>
   <a target="_blank" href='https://github.com/dromara/soul'>
        <img src="https://img.shields.io/github/forks/dromara/soul.svg" alt="github forks"/>
   </a>
   <a target="_blank" href='https://github.com/dromara/soul'>
        <img src="https://img.shields.io/github/stars/dromara/soul.svg" alt="github stars"/>
   </a>
   <a target="_blank" href='https://github.com/dromara/soul'>
        <img src="https://img.shields.io/github/contributors/dromara/soul.svg" alt="github contributors"/>
   </a>     
   <a href="https://github.com/Dromara/soul">
        <img src="https://tokei.rs/b1/github/Dromara/soul?category=lines"/>
   </a>
   <a target="_blank" href="https://codecov.io/gh/dromara/soul">
        <img src="https://codecov.io/gh/dromara/soul/branch/master/graph/badge.svg" />
   </a>
</p>
<br/>

--------------------------------------------------------------------------------

# 架构

 ![](https://yu199195.github.io/images/soul/soul-framework.png)  

--------------------------------------------------------------------------------

# 结构图

![](https://dromara.org/img/soul/activite/soul-xmind.png)

--------------------------------------------------------------------------------

# 模块

 * soul-admin : 插件和其他信息配置的管理后台

 * soul-bootstrap : 用于启动项目，用户可以参考

 * soul-client : 用户可以使用 Spring MVC，Dubbo，Spring Cloud 快速访问
 
 * soul-disruptor : 基于disruptor的封装
  
 * soul-register-center : 为soul-client提供各种rpc接入注册中心的支持
  
 * soul-common : 框架的通用类

 * soul-dist : 构建项目

 * soul-metrics : prometheus（普罗米修斯）实现的 metrics

 * soul-plugin : Soul 支持的插件集合

 * soul-spi : 定义 Soul spi

 * soul-spring-boot-starter : 支持 spring boot starter

 * soul-sync-data-center : 提供 ZooKeeper，HTTP，WebSocket，Nacos 的方式同步数据

 * soul-examples : RPC 示例项目

 * soul-web : 包括插件、请求路由和转发等的核心处理包

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

无论请求何时进入，Soul 会通过响应链执行所有已打开的插件。

插件是 Soul 的灵魂，并且插件也是可扩展和热插拔的。

不同的插件实现不同的功能。

当然，用户也可以定制化插件去满足他们自己的需求。

如果你有定制化插件的需求，请参看这里：[custom-plugin](https://dromara.org/zh/projects/soul/custom-plugin/)

--------------------------------------------------------------------------------

# 选择器和规则

选择器和规则会根据 HTTP 的请求头来路由你的请求。

选择器是你的第一个路由，它是粗粒度的，举个例子，模块级别。

规则是你的第二个路由，即你认为你的请求应该做什么，举个例子，模块中的方法级别。

选择器和规则只匹配一次，然后返回匹配。因此，最粗粒度应排在最后。

--------------------------------------------------------------------------------

# 数据缓存 & 数据同步

所有的数据都被缓存在 JVM 的 ConcurrentHashMap 中，所以它非常快。

当用户在后台界面改变配置信息时，Soul 通过监听 ZooKeeper node，WebSocket push，HTTP longPull 来动态更新缓存。

  ![](https://yu199195.github.io/images/soul/soul-config-processor.png)

  ![](https://yu199195.github.io/images/soul/config-strage-processor.png)

--------------------------------------------------------------------------------

# 必要条件

   * JDK 1.8+

--------------------------------------------------------------------------------

# 关于

Soul 已经被很多公司广泛使用在越来越多的业务系统，它能以高性能和灵活性让我们方便快捷的集成自己的服务和 API 。

在中国的双 11 购物狂欢节中，Soul集群集群成功支撑了海量的互联网业务。

--------------------------------------------------------------------------------

# 文档& 网站

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://dromara.org/projects/soul/overview)
[![CN doc](https://img.shields.io/badge/document-Chinese-blue.svg)](https://dromara.org/zh/projects/soul/overview)

--------------------------------------------------------------------------------

# Github stars趋势

[![Stargazers over time](https://starchart.cc/Dromara/soul.svg)](https://starchart.cc/Dromara/soul)

--------------------------------------------------------------------------------

# Gitee stars趋势

[![Giteye chart](https://chart.giteye.net/gitee/dromara/soul/UZUPQEPC.png)](https://giteye.net/chart/UZUPQEPC)

--------------------------------------------------------------------------------

# 目前已知用户

为了便于登记，欢迎已经使用了 Soul 的公司在 [https://github.com/Dromara/soul/issues/68](https://github.com/Dromara/soul/issues/68) 注册。（仅适用于开源用户）

<table>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/joyy.png"  width="1800" height="90" alt="yy"/>
      <td><img src="https://yu199195.github.io/images/soul/users/mihoyo.jpg"  width="1800" height="90" alt="mihoyo"/>
      <td><img src="https://yu199195.github.io/images/soul/users/keking.png"  width="1800" height="90" alt="kk group"/>
      <td><img src="https://yu199195.github.io/images/soul/users/shansong.jpg"  width="1800" height="90" alt="shansong"/>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/sibu.jpg"  width="1800" height="90" alt="sibu group"/>
      <td><img src="https://yu199195.github.io/images/soul/users/guojiadianwang.jpg"  width="1800" height="90" alt="guojiadianwang"/>
      <td><img src="https://yu199195.github.io/images/soul/users/caibeike.png"  width="1800" height="90" alt="caibeike"/>
      <td><img src="https://yu199195.github.io/images/soul/users/jiangsuyonggang.jpg"  width="1800" height="90" alt="jiangsuyonggang"/>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/fangfutong.png"  width="1800" height="90" alt="fangfutong"/>
      <td><img src="https://yu199195.github.io/images/soul/users/lixiang.jpg"  width="1800" height="90" alt="lixiang"/>
      <td><img src="https://yu199195.github.io/images/soul/users/kaipuyun.png"  width="1800" height="90" alt="kaipuyun"/>
      <td><img src="https://yu199195.github.io/images/soul/users/songda.png"  width="1800" height="90" alt="songda"/>
     </tr>
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/aoyou.jpg"  width="1800" height="90" alt="aoyou"/>
      <td><img src="https://yu199195.github.io/images/soul/users/cheyipai.jpg"  width="1800" height="90" alt="cheyipai"/>
      <td><img src="https://yu199195.github.io/images/soul/users/caomao.jpg"  width="1800" height="90" alt="caomao"/>
      <td><img src="https://yu199195.github.io/images/soul/users/zuyun.jpg"  width="1800" height="90" alt="zuyun"/>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/hezhi.png"  width="1800" height="90" alt="hezhi"/>
      <td><img src="https://yu199195.github.io/images/soul/users/qidianyun.jpg"  width="1800" height="90" alt="qidianyun"/>
      <td><img src="https://yu199195.github.io/images/soul/users/wanwei.gif"  width="1800" height="90" alt="wanwei"/>
      <td><img src="https://yu199195.github.io/images/soul/users/wuyiyuntong.jpg"  width="1800" height="90" alt="wuyiyuntong"/>
    </tr>            
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/haokangzaijia.jpg"  width="1800" height="90" alt="haokangzaijia"/>
      <td><img src="https://yu199195.github.io/images/soul/users/caissa.jpg"  width="1800" height="90" alt="caissa"/>
      <td><img src="https://yu199195.github.io/images/soul/users/deepBule.png"  width="1800" height="90" alt="deepBule"/>
      <td><img src="https://yu199195.github.io/images/soul/users/anka.png"  width="1800" height="90" alt="anka"/>
    </tr>
  </tbody>     
  <tbody>
    <tr>
      <td><img src="https://dromara.org/img/users/jd_logo.png"  width="1800" height="90" alt="jd"/>
      <td><img src="https://yu199195.github.io/images/soul/users/minglamp.jpeg"  width="1800" height="90" alt="minglamp"/>
      <td><img src="https://yu199195.github.io/images/soul/users/webuy.jpg"  width="1800" height="90" alt="webuy"/>
      <td><img src="https://dromara.org/img/users/cass.png"  width="1800" height="90" alt="cass"/>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><img src="https://dromara.org/img/users/songguo.png"  width="1800" height="90" alt="songguo"/>
      <td><img src="https://dromara.org/img/users/lianlian.png"  width="1800" height="90" alt="lianlian"/>
      <td><img src="https://dromara.org/img/users/dasouche.png"  width="1800" height="90" alt="dasouche"/>
      <td><img src="https://dromara.org/img/users/weimai.png"  width="1800" height="90" alt="weimai"/>
    </tr>
  </tbody>
</table>

#贡献者列表

`Soul` 遵循 `Apache-2.0` 开源协议，欢迎大家提交 `PR` 或 `Issue`。

如果要为项目做出贡献，请查看 [贡献指南](https://dromara.org/zh/projects/soul/contributor/e)。

感谢每一位为 `Soul` 贡献代码的朋友。

[![Giteye chart](https://chart.giteye.net/gitee/dromara/soul/2ZKY3P9W.png)](https://giteye.net/chart/2ZKY3P9W)

# 支持

<table>
  <thead>
    <th>微信公众号</th>
    <th>QQ 交流群</th>
    <th>芋道源码</th>
    <th>JetBrains</th>
  </thead>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/public.jpg"   alt="微信公众号"/>
      <td><img src="https://yu199195.github.io/images/soul-qq.png"  alt="QQ 交流群"/>
      <td><img src="http://www.iocoder.cn/images/common/erweima.jpg"  alt="芋道源码"/>
      <td><img src="https://yu199195.github.io/images/jetbrains.svg"  alt="JetBrains"/>
    </tr>
  </tbody>
</table>




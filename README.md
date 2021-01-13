<p align="center" >
    <a href="https://dromara.org"><img src="https://yu199195.github.io/images/soul/soul-logo.png" width="45%"></a>
</p>
<p align="center">
  <strong>Scalable,High Performance,Responsive API Gateways</strong>
</p>
<p align="center">
  <a href="https://dromara.org">https://dromara.org/</a>
</p>

<p align="center">
  English | <a href="https://github.com/dromara/soul/blob/master/README_CN.md">简体中文</a>
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
    <a target="_blank" href='https://gitee.com/shuaiqiyu/soul/stargazers'>
        <img src='https://gitee.com/shuaiqiyu/soul/badge/star.svg?theme=gvp' alt='gitee stars'/>
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

# Architecture
 
 ![](https://yu199195.github.io/images/soul/soul-framework.png)  
 
-------------------------------------------------------------------------------- 
  
# Execution Flow
 
 ![](https://yu199195.github.io/images/soul/soul-handler.png)
 
--------------------------------------------------------------------------------  
  
# Modules

 * soul-admin : plugins and other information configuration management background
 
 * soul-bootstrap : with the startup project, users can refer to
 
 * soul-client : user fast access with Spring MVC, Dubbo, Spring Cloud.
  
 * soul-common : framework common class
 
 * soul-dist : build project

 * soul-metrics : metrics impl by prometheus.
 
 * soul-plugin : Soul provider plugin collection.
 
 * soul-spi : Soul spi define.
 
 * soul-spring-boot-starter : support for the spring boot starter
 
 * soul-sync-data-center : provider ZooKeeper, HTTP, WebSocket, Nacos to sync data
 
 * soul-examples : the RPC examples project
 
 * soul-web : core processing packages including plugins, request routing and forwarding, and so on
 
--------------------------------------------------------------------------------   
 
# Features

   * Soul provides plugins such as current limiting, fusing, forwarding, routing monitoring and so on.
   
   * Seamless docking with HTTP, RESTful, WebSocket, Dubbo and Spring Cloud.
   
   * Plug-in hot plug, users can customize the development.
   
   * Selectors and rules are dynamically configured for flexible matching.

   * Support for cluster deployment.
   
   * Support A/B test and grayscale publishing.
   
--------------------------------------------------------------------------------  
 
# Plugin

 Whenever a request comes in, Soul executes all open plugins through the chain of responsibility.
 
 As the heart of Soul, plugins are extensible and hot-pluggable.
 
 Different plugins do different things.
 
 Of course, users can also customize plugins to meet their own needs.
 
 If you want to customize, see [plugin-extend](https://dromara.org/website/zh-cn/docs/soul/extend.html)
 
--------------------------------------------------------------------------------  
 
# Selector & rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do,For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
 
--------------------------------------------------------------------------------  
   
# Data Caching & Data Sync
 
  All data have been cached using ConcurrentHashMap in the JVM, so it's very fast.
  
  When the user is managing changes in the background, Soul dynamically updates the cache by listening to the ZooKeeper node, WebSocket push, HTTP longPull.
  
  ![](https://yu199195.github.io/images/soul/soul-config-processor.png)
  
  ![](https://yu199195.github.io/images/soul/config-strage-processor.png)

--------------------------------------------------------------------------------    

# Prerequisite
 
   * JDK 1.8+
   
   * MySQL
   
--------------------------------------------------------------------------------     
   
# About
  
   Soul has been used in our production environment,its performance and flexibility allow us to use up very cool.
   
   In double 11, we deployed 6 clusters, which supported a large volume of our business.
   
--------------------------------------------------------------------------------  
    
# Document & Website

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://dromara.org/website/en-us/docs/soul/soul.html)
[![CN doc](https://img.shields.io/badge/document-Chinese-blue.svg)](https://dromara.org/website/zh-cn/docs/soul/soul.html)
  
--------------------------------------------------------------------------------  
        
# Stargazers over time

[![Stargazers over time](https://starchart.cc/Dromara/soul.svg)](https://starchart.cc/Dromara/soul)

--------------------------------------------------------------------------------  

# Videos

* [evn setup 01 ](http://www.iqiyi.com/w_19s6521605.html)

* [evn setup 02 ](http://www.iqiyi.com/w_19s65203ap.html)

* [source code debug](http://www.iqiyi.com/w_19s650tbol.html)

* [plugins](http://www.iqiyi.com/w_19s651zyo9.html)

--------------------------------------------------------------------------------  

# Known Users

In order of registration, More access companies are welcome to register at [https://github.com/Dromara/soul/issues/68](https://github.com/Dromara/soul/issues/68) (For open source users only)

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
</table>

# Support  

<table>
  <thead>
    <th>WeChat</th>
    <th>QQ</th>
    <th>Taro Source</th>
  </thead>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/public.jpg"   alt="微信公众号"/>
      <td><img src="https://yu199195.github.io/images/soul-qq.png"  alt="QQ 交流群"/>
      <td><img src="http://www.iocoder.cn/images/common/erweima.jpg"  alt="芋道源码"/>
    </tr>
  </tbody>
</table>
  
 

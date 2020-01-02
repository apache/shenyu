# [Soul](https://dromara.org)

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/4367ffad5b434b7e8078b3a68cc6398d)](https://www.codacy.com/app/yu199195/soul?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=Dromara/soul&amp;utm_campaign=Badge_Grade)
[![Total lines](https://tokei.rs/b1/github/Dromara/soul?category=lines)](https://github.com/Dromara/soul)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg?label=license)](https://github.com/Dromara/soul/blob/master/LICENSE)
[![Maven Central](https://img.shields.io/maven-central/v/org.dromara/soul.svg?label=maven%20central)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.dromara%22%20AND%soul)
[![QQ群](https://img.shields.io/badge/chat-on%20QQ-ff69b4.svg?style=flat-square)](https://shang.qq.com/wpa/qunwpa?idkey=03bbb6f74b3257989316c0a8cf07cec117314dbdfe4fa7a20870b298b7db2c3b)


### Reactive gateway based on webflux

# Architecture
 
 ![](https://yu199195.github.io/images/soul/soul-framework.png)  
  
# Execution Flow
 
 ![](https://yu199195.github.io/images/soul/soul-handler.png)
  
# Modules

 * soul-admin : Plug-in and other information configuration management background
 
 * soul-bootstrap : With the startup project, users can refer to
 
 * soul-common :  Framework common class
 
 * soul-configuration : zookeeper configuration project
 
 * soul-spring-boot-starter : Support for the spring boot starter
 
 * soul-web : Core processing packages include plug-ins, request routing and forwarding, and so on
 
 * soul-extend-demo : Demo of the extension point
 
 * soul-test : the rpc test project

# Features

   * It provides plugins such as current limiting, fusing, forwarding, routing monitoring and so on.
   
   * Seamless docking with HTTP,Restful,websocket,dubbo and springcloud.
   
   * Plug-in hot plug, users can customize the development.
   
   * Selectors and rules are dynamically configured for flexible matching.

   * Support for cluster deployment.
   
   * Support A/B test and grayscale publishing。
   

# Plugin

 Whenever a request comes in ,Soul Execute all open plug-ins through the chain of responsibility.
 
 Plugins are the heart of soul And plug-ins are extensible and hot-pluggable.
 
 Different plug-ins do different things 
 
 Of course, users can also customize plug-ins to meet their own needs.
 
 If you want to customize, see [plugin-extend](https://dromara.org/website/zh-cn/docs/soul/extend.html)
 

# Selector & rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do,For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
   
  
# Data Caching  & Data Sync
 
  All data is cached ConcurrentHashMap in the JVM So it's very fast.
  
  When the user is managing changes in the background,
  
  Soul dynamically updates the cache by listening to the zookeeper node, websocket push,http longPull.
  
  ![Data Sync](https://bestkobe.gitee.io/images/soul/soul-config-processor.png?_t=201908032316)
  
  ![Sync Flow](https://bestkobe.gitee.io/images/soul/config-strage-processor.png?_t=201908032339)
 
# Quick Start
 * get `soul-admin.jar`
 
```
> wget  https://yu199195.github.io/jar/soul-admin.jar
```

* start `soul-admin.jar`
```java
> java -jar soul-admin.jar --spring.datasource.url="jdbc:mysql://your ip:3306/soul?useUnicode=true&characterEncoding=utf-8&zeroDateTimeBehavior=CONVERT_TO_NULL&failOverReadOnly=false&autoReconnect=true&useSSL=false"  
  --spring.datasource.username='you username'  --spring.datasource.password='you password'
```
* visit : http://localhost:9095/index.html  username:admin  password :123456

* get `soul-bootstrap.jar`

```java
> wget  https://yu199195.github.io/jar/soul-bootstrap.jar
```

*  start `soul-bootstrap.jar`  

```xml
 java -jar soul-bootstrap.jar
```

# Prerequisite
 
   * JDK 1.8+
   
   * Mysql
   
# About & Document
  
   Soul Has been used in our production environment,Its performance and flexibility allow us to use up very cool.
   
   In double 11, we deployed 6 clusters, which supported a large volume of our business.
   
   If you want to use it, you can see [Document](https://dromara.org/website/zh-cn/docs/soul/soul.html)
        
# Stargazers over time

[![Stargazers over time](https://starchart.cc/Dromara/soul.svg)](https://starchart.cc/Dromara/soul)

# Videos

* [evn setup 01 ](http://www.iqiyi.com/w_19s6521605.html)

* [evn setup 02 ](http://www.iqiyi.com/w_19s65203ap.html)

* [source code debug](http://www.iqiyi.com/w_19s650tbol.html)

* [plugins](http://www.iqiyi.com/w_19s651zyo9.html)

# Known Users

按照登记顺序排序，更多接入公司，欢迎在[https://github.com/Dromara/soul/issues/68](https://github.com/Dromara/soul/issues/68)登记（仅供开源用户参考）


<table>
  <thead>
    <th>广州思埠集团有限公司</th>
    <th>上海凯京信达科技集团有限公司</th>
    <th>上海彩贝壳科技有限公司</th>
    <th>江苏永钢集团有限公司</th>
  </thead>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/soul/users/sibu.jpg"  width="1800" height="90" alt="思埠集团"/>
      <td><img src="https://yu199195.github.io/images/soul/users/keking.png"  width="1800" height="90" alt="凯京集团"/>
      <td><img src="https://yu199195.github.io/images/soul/users/caibeike.png"  width="1800" height="90" alt="彩贝壳"/>
      <td><img src="https://yu199195.github.io/images/soul/users/jiangsuyonggang.jpg"  width="1800" height="90" alt="江苏永钢集团"/>
    </tr>
  </tbody>
  <thead>
      <th>上海方付通科技有限公司 </th>
      <th>上海理想信息产业集团有限公司</th>
      <th>北京开普云科技有限公司</th>
      <th>武汉颂大教育科技股份有限公司</th>
   </thead>
   <tbody>
       <tr>
         <td><img src="https://yu199195.github.io/images/soul/users/fangfutong.png"  width="1800" height="90" alt="上海方付通科技有限公司"/>
         <td><img src="https://yu199195.github.io/images/soul/users/lixiang.jpg"  width="1800" height="90" alt="上海理想信息产业集团有限公司"/>
         <td><img src="https://yu199195.github.io/images/soul/users/kaipuyun.png"  width="1800" height="90" alt="彩贝壳"/>
         <td><img src="https://yu199195.github.io/images/soul/users/songda.png"  width="1800" height="90" alt="江苏永钢集团"/>
       </tr>
    </tbody>
     <thead>
          <th>浙江翱游科技有限公司 </th>
          <th>车易拍(北京)汽车技术服务有限公司</th>
          <th>上海草帽科技</th>
          <th>深圳竹云科技有限公司</th>
       </thead>
       <tbody>
           <tr>
             <td><img src="https://yu199195.github.io/images/soul/users/aoyou.jpg"  width="1800" height="90" alt="浙江翱游科技有限公司"/>
             <td><img src="https://yu199195.github.io/images/soul/users/cheyipai.jpg"  width="1800" height="90" alt="车易拍(北京)汽车技术服务有限公司"/>
             <td><img src="https://yu199195.github.io/images/soul/users/caomao.jpg"  width="1800" height="90" alt="上海草帽科技"/>
             <td><img src="https://yu199195.github.io/images/soul/users/zuyun.jpg"  width="1800" height="90" alt="深圳竹云科技有限公司"/>
           </tr>
        </tbody>
</table>


# Support  

<table>
  <thead>
    <th>芋道源码</th>
    <th>微信公众号</th>
    <th>QQ交流群</th>
    <th>jetbrains</th>
  </thead>
  <tbody>
    <tr>
      <td><img src="http://www.iocoder.cn/images/common/erweima.jpg"  alt="芋道源码"/>
      <td><img src="https://yu199195.github.io/images/public.jpg"   alt="微信公众号"/>
      <td><img src="https://yu199195.github.io/images/soul-qq.png"  alt="QQ交流群"/>
      <td><img src="https://yu199195.github.io/images/jetbrains.svg" alt="jetbrains"/>
    </tr>
  </tbody>
</table>
  
 
 
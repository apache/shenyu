<p align="center" >
    <a href="https://dromara.org"><img src="https://yu199195.github.io/images/soul/soul-logo.png" width="45%"></a>
</p>
<p align="center">
  <strong>易扩展，高性能，响应式API网关</strong>
</p>
<p align="center">
  <a href="https://dromara.org">https://dromara.org/</a>
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
        <img src="https://github.com/dromara/soul/workflows/build/badge.svg" />
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

 * soul-admin : Plug-in and other information configuration management background
 
 * soul-bootstrap : With the startup project, users can refer to
 
 * soul-client : User fast access with springMvc,dubbo,springCloud
  
 * soul-common : Framework common class
 
 * soul-dist : build project

 * soul-metrics : metrics impl by prometheus.
 
 * soul-plugin : soul provider plugin collection.
 
 * soul-spi : soul spi define.
 
 * soul-spring-boot-starter : Support for the spring boot starter
 
 * soul-sync-data-center : provider zookeeper, http, websocket, nacos to sync data
 
 * soul-test : the rpc test project
 
 * soul-web : Core processing packages include plug-ins, request routing and forwarding, and so on
 
--------------------------------------------------------------------------------   
 
# Features

   * It provides plugins such as current limiting, fusing, forwarding, routing monitoring and so on.
   
   * Seamless docking with HTTP,Restful,websocket,dubbo and springcloud.
   
   * Plug-in hot plug, users can customize the development.
   
   * Selectors and rules are dynamically configured for flexible matching.

   * Support for cluster deployment.
   
   * Support A/B test and grayscale publishing。
   
--------------------------------------------------------------------------------  
 
# Plugin

 Whenever a request comes in ,Soul Execute all open plug-ins through the chain of responsibility.
 
 Plugins are the heart of soul And plug-ins are extensible and hot-pluggable.
 
 Different plug-ins do different things 
 
 Of course, users can also customize plug-ins to meet their own needs.
 
 If you want to customize, see [plugin-extend](https://dromara.org/website/zh-cn/docs/soul/extend.html)
 
--------------------------------------------------------------------------------  
 
# Selector & rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do,For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
 
--------------------------------------------------------------------------------  
   
# Data Caching & Data Sync
 
  All data is cached ConcurrentHashMap in the JVM So it's very fast.
  
  When the user is managing changes in the background,
  
  Soul dynamically updates the cache by listening to the zookeeper node, websocket push,http longPull.
  
  ![Data Sync](https://bestkobe.gitee.io/images/soul/soul-config-processor.png?_t=201908032316)
  
  ![Sync Flow](https://bestkobe.gitee.io/images/soul/config-strage-processor.png?_t=201908032339)

--------------------------------------------------------------------------------    

# Prerequisite
 
   * JDK 1.8+
   
   * Mysql
   
--------------------------------------------------------------------------------     
   
# About
  
   Soul Has been used in our production environment,Its performance and flexibility allow us to use up very cool.
   
   In double 11, we deployed 6 clusters, which supported a large volume of our business.
   
--------------------------------------------------------------------------------  
    
# Document & Website

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://dromara.org/website/en-us/docs/soul/soul.html)
[![CN doc](https://img.shields.io/badge/文档-中文版-blue.svg)](https://dromara.org/website/zh-cn/docs/soul/soul.html)
  
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
     <thead>
               <th>深圳盒知科技 </th>
               <th>杭州奇点云科技</th>
               <th>深圳万威科技</th>
               <th>武汉物易云通网络科技有限公司</th>
      </thead>
      <tbody>
                <tr>
                  <td><img src="https://yu199195.github.io/images/soul/users/hezhi.png"  width="1800" height="90" alt="深圳盒知科技"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/qidianyun.jpg"  width="1800" height="90" alt="杭州奇点云科技"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/wanwei.gif"  width="1800" height="90" alt="万威科技"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/wuyiyuntong.jpg"  width="1800" height="90" alt="武汉物易云通网络科技有限公司"/>
                </tr>
      </tbody>
      <thead>
               <th>好慷在家</th>
               <th>闪送科技</th>
               <th>国家电网</th>
               <th>凯撒旅游</th>
      </thead>
      <tbody>
                <tr>
                  <td><img src="https://yu199195.github.io/images/soul/users/haokangzaijia.jpg"  width="1800" height="90" alt="好慷在家"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/shansong.jpg"  width="1800" height="90" alt="闪送科技"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/guojiadianwang.jpg"  width="1800" height="90" alt="国家电网"/>
                  <td><img src="https://yu199195.github.io/images/soul/users/caissa.jpg"  width="1800" height="90" alt="凯撒旅游"/>
                </tr>
      </tbody>     
</table>


# Support  

<table>
  <thead>
    <th>微信公众号</th>
    <th>QQ交流群</th>
    <th>芋道源码</th>
  </thead>
  <tbody>
    <tr>
      <td><img src="https://yu199195.github.io/images/public.jpg"   alt="微信公众号"/>
      <td><img src="https://yu199195.github.io/images/soul-qq.png"  alt="QQ交流群"/>
      <td><img src="http://www.iocoder.cn/images/common/erweima.jpg"  alt="芋道源码"/>
    </tr>
  </tbody>
</table>
  
 

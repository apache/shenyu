<p align="center" >
    <a href="https://shenyu.apache.org/"><img src="https://shenyu.apache.org/img/logo/apache-shenyu.png" width="45%"></a>
</p>
<p align="center">
  <strong>Scalable, High Performance, Responsive API Gateway Solution for all MicroServices</strong>
</p>
<p align="center">
  <a href="https://shenyu.apache.org/">https://shenyu.apache.org/</a>
</p>

<p align="center">
  English | <a href="https://github.com/apache/shenyu/blob/master/README_CN.md">简体中文</a>
</p>

<p align="center">
    <a target="_blank" href="https://search.maven.org/search?q=g:org.apache.shenyu%20AND%20a:shenyu">
        <img src="https://img.shields.io/maven-central/v/org.apache.shenyu/shenyu.svg?label=maven%20central" />
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

# Architecture
 
 ![](https://shenyu.apache.org/img/architecture/shenyu-framework.png)  
 
-------------------------------------------------------------------------------- 
    
 
# Features

   * ShenYu provides ability such as current limiting, fusing, forwarding, routing monitoring and so on by its plugins.
   
   * Support HTTP, RESTFul, WebSocket, Dubbo, GRPC, Tars and Spring Cloud Proxy.
   
   * Plug-in hot plug, users can customize the development.
   
   * Selectors and rules are dynamically configured for flexible matching.

   * Support for cluster deployment.
   
   * Support A/B test and grayscale publishing.
   
--------------------------------------------------------------------------------  
 
# Plugin

 Whenever a request comes in, ShenYu will execute it by all enabled plugins through the chain of responsibility.
 
 As the heart of ShenYu, plugins are extensible and hot-pluggable.
 
 Different plugins do different things.
 
 Of course, users can also customize plugins to meet their own needs.
 
 If you want to customize, see [custom-plugin](https://shenyu.apache.org/docs/developer/custom-plugin/)
 
--------------------------------------------------------------------------------  
 
# Selector & rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do. For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
 
--------------------------------------------------------------------------------  
   
# Data Caching & Data Sync
 
  Since all data have been cached using ConcurrentHashMap in the JVM, it's very fast.
  
  When user have changed the configuration in the background management, ShenYu will dynamically updates its cache by listening to the ZooKeeper node, WebSocket push, HTTP longPull.
  
  ![](https://shenyu.apache.org/img/shenyu/dataSync/shenyu-config-processor-en.png)
  
  ![](https://shenyu.apache.org/img/shenyu/dataSync/config-strategy-processor-en.png)

--------------------------------------------------------------------------------    

# Prerequisite
 
   * JDK 1.8+
   
--------------------------------------------------------------------------------    
    
# Document & Website

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://shenyu.apache.org/docs/index)
[![CN doc](https://img.shields.io/badge/document-Chinese-blue.svg)](https://shenyu.apache.org/zh/docs/index/)
  
--------------------------------------------------------------------------------  
        
# Stargazers over time

[![Stargazers over time](https://starchart.cc/apache/incubator-shenyu.svg)](https://starchart.cc/apache/incubator-shenyu.svg)

--------------------------------------------------------------------------------  

# Known Users

In order of registration, More access companies are welcome to register at [https://github.com/apache/incubator-shenyu/issues/68](https://github.com/apache/incubator-shenyu/issues/68) (For open source users only)

All Users : [Known Users](https://shenyu.apache.org/community/user-registration)

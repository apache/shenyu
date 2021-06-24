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

# Architecture
 
 ![](https://shenyu.apache.org/img/architecture/shenyu-framework.png)  
 
-------------------------------------------------------------------------------- 
  
# Mind maps
 
 ![](https://shenyu.apache.org/img/shenyu/activite/shenyu-xmind.png)
 
--------------------------------------------------------------------------------  
  
# Modules

 * shenyu-admin : plugins and other configuration information management background
 
 * shenyu-bootstrap : with the startup project, users can refer to
 
 * shenyu-client : user fast access with Spring MVC, Dubbo, Spring Cloud.
  
 * shenyu-common : framework common class
 
 * shenyu-disruptor : based on disruptor Enclosure
 
 * shenyu-register-center : rpc type register for shenyu-client
 
 * shenyu-dist : build project

 * shenyu-metrics : metrics impl by prometheus.
 
 * shenyu-plugin : ShenYu provider plugin collection.
 
 * shenyu-spi : ShenYu spi define.
 
 * shenyu-spring-boot-starter : support for the spring boot starter
 
 * shenyu-sync-data-center : provider ZooKeeper, HTTP, WebSocket, Nacos to sync data
 
 * shenyu-examples : the RPC examples project
 
 * shenyu-web : core processing packages including plugins, request routing and forwarding, and so on
 
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
 
 If you want to customize, see [custom-plugin](https://shenyu.apache.org/projects/shenyu/custom-plugin/)
 
--------------------------------------------------------------------------------  
 
# Selector & rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do. For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
 
--------------------------------------------------------------------------------  
   
# Data Caching & Data Sync
 
  Since all data have been cached using ConcurrentHashMap in the JVM, it's very fast.
  
  When user have changed the configuration in the background management, ShenYu wiil dynamically updates its cache by listening to the ZooKeeper node, WebSocket push, HTTP longPull.
  
  ![](https://yu199195.github.io/images/soul/soul-config-processor.png)
  
  ![](https://yu199195.github.io/images/soul/config-strage-processor.png)

--------------------------------------------------------------------------------    

# Prerequisite
 
   * JDK 1.8+
   
--------------------------------------------------------------------------------     
   
# About
  
   ShenYu has been used widely in more and more systems in many companies, and it's simple and convenient to integrate Services/APIs with the high performance and flexibility.
   
   In double eleven online shopping carnival of China, ShenYu clusters successfully supported a large volume of internet business.
   
--------------------------------------------------------------------------------  
    
# Document & Website

[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://shenyu.apache.org/projects/shenyu/overview)
[![CN doc](https://img.shields.io/badge/document-Chinese-blue.svg)](https://shenyu.apache.org/zh/projects/shenyu/overview)
  
--------------------------------------------------------------------------------  
        
# Stargazers over time

[![Stargazers over time](https://starchart.cc/apache/incubator-shenyu.svg)](https://starchart.cc/apache/incubator-shenyu.svg)

--------------------------------------------------------------------------------  

# Known Users

In order of registration, More access companies are welcome to register at [https://github.com/apache/incubator-shenyu/issues/68](https://github.com/apache/incubator-shenyu/issues/68) (For open source users only)

All Users : [Known Users](https://shenyu.apache.org/awesome/)

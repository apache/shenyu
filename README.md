# [Soul](https://dromara.org)

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/4367ffad5b434b7e8078b3a68cc6398d)](https://www.codacy.com/app/yu199195/soul?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=Dromara/soul&amp;utm_campaign=Badge_Grade)
[![Total lines](https://tokei.rs/b1/github/Dromara/soul?category=lines)](https://github.com/Dromara/soul)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg?label=license)](https://github.com/Dromara/soul/blob/master/LICENSE)
[![Build Status](https://travis-ci.org/Dromara/soul.svg?branch=master)](https://travis-ci.org/Dromara/soul)
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
   
   * Seamless docking with HTTP, dubbo, and springcloud.
   
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
   
  
# Data Caching 
 
  All data is cached HashMap in the JVM So it's very fast.
  
  When the user is managing changes in the background,
  
  Soul dynamically updates the cache by listening to the zookeeper node.
  
  This solution might rely on zookeeper,It may be replaced in the future [issues](https://github.com/Dromara/soul/issues/35)
  
  Node design for zookeeper [zookeeper-node](https://dromara.org/website/zh-cn/docs/soul/config.html)
 

# Prerequisite
 
   * JDK 1.8+

   * Zookeeper
   
   * Mysql
   
# About
  
   Soul Has been used in our production environment,Its performance and flexibility allow us to use up very cool.
   
   In double 11, we deployed 6 clusters, which supported a large volume of our business.
   
   If you want to use it, you can see [Quick Start](https://dromara.org/website/zh-cn/docs/soul/soul.html)
     
# Stargazers over time

[![Stargazers over time](https://starchart.cc/Dromara/soul.svg)](https://starchart.cc/Dromara/soul)

  
# Support

 ![](https://yu199195.github.io/images/soul-qq.png)
  

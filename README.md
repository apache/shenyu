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

# Why named ShenYu

ShenYu (神禹) is the honorific name for our ancient monarch Xia Yu (also known in later times as Da Yu), 
who left behind the touching story of the three times he crossed the Yellow River for the benefit of the people and successfully managed the flooding of the river. 
He is known as one of the three greatest kings of ancient China, along with Yao and Shun.

   * Firstly, the name ShenYu is to promote the traditional virtues of our Chinese civilisation.

   * Secondly, the most important thing about the gateway is the governance of the traffic.

   * Finally, the community will do things in a fair, just, open and meritocratic way, paying tribute to ShenYu while also conforming to the Apache Way.

-------------------------------------------------------------------------------- 

# Features

   * ShenYu provides ability such as current limiting, fusing, forwarding, routing, monitoring and so on by its plugins.
   
   * Support HTTP, RESTFul, WebSocket, Dubbo, gRPC, Tars and Spring Cloud Proxy.
   
   * Plugin hot plug, users can customize the development.
   
   * Selectors and rules are dynamically configured for flexible matching.

   * Support for cluster deployment.
   
   * Support A/B test and grayscale publishing.
   
--------------------------------------------------------------------------------  

# Quick Start (docker)

### Run Apache ShenYu Admin

```
> docker pull apache/shenyu-admin
> docker network create shenyu
> docker run -d -p 9095:9095 --net shenyu apache/shenyu-admin
```

### Run Apache ShenYu Bootstrap

```
> docker network create shenyu
> docker pull apache/shenyu-bootstrap
> docker run -d -p 9195:9195 --net shenyu apache/shenyu-bootstrap
```                       

### Set router

* Real requests  ：http://127.0.0.1:8080/helloworld,

```json
{
  "name" : "Shenyu",
  "data" : "hello world"
}
```

* Set routing rules (Standalone)

Add `localKey: 123456` to Headers. If you need to customize the localKey, you can use the sha512 tool to generate the key based on plaintext and update the `shenyu.local.sha512Key` property.

```
curl --location --request POST 'http://localhost:9195/shenyu/plugin/selectorAndRules' \
--header 'Content-Type: application/json' \
--header 'localKey: 123456' \
--data-raw '{
    "pluginName": "divide",
    "selectorHandler": "[{\"upstreamUrl\":\"127.0.0.1:8080\"}]",
    "conditionDataList": [{
        "paramType": "uri",
        "operator": "match",
        "paramValue": "/**"
    }],
    "ruleDataList": [{
        "ruleHandler": "{\"loadBalance\":\"random\"}",
        "conditionDataList": [{
            "paramType": "uri",
            "operator": "match",
            "paramValue": "/**"
        }]
    }]
}'
```

* Proxy request ：http://localhost:9195/helloworld 

```json
{
  "name" : "Shenyu",
  "data" : "hello world"
}
```
--------------------------------------------------------------------------------

# Plugin

 Whenever a request comes in, ShenYu will execute it by all enabled plugins through the chain of responsibility.
 
 As the heart of ShenYu, plugins are extensible and hot-pluggable.
 
 Different plugins do different things.
 
 Of course, users can also customize plugins to meet their own needs.
 
 If you want to customize, see [custom-plugin](https://shenyu.apache.org/docs/developer/custom-plugin/) .
 
--------------------------------------------------------------------------------  
 
# Selector & Rule 

  According to your HTTP request headers, selectors and rules are used to route your requests.
  
  Selector is your first route, It is coarser grained, for example, at the module level.
  
  Rule is your second route and what do you think your request should do. For example a method level in a module.
  
  The selector and the rule match only once, and the match is returned. So the coarsest granularity should be sorted last.
 
--------------------------------------------------------------------------------  
   
# Data Caching & Data Sync
 
  Since all data have been cached using ConcurrentHashMap in the JVM, it's very fast.
  
  ShenYu dynamically updates the cache by listening to the ZooKeeper node (or WebSocket push, HTTP long polling) when the user changes configuration information in the background management.
  
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

In order of registration, More access companies are welcome to register at [https://github.com/apache/incubator-shenyu/issues/68](https://github.com/apache/incubator-shenyu/issues/68) (For open source users only) .

All Users : [Known Users](https://shenyu.apache.org/community/user-registration)

---
title: monitor插件
keywords: monitor
description: monitor插件
---

## 说明

* monitor插件是网关用来监控自身运行状态（JVM相关），请求的响应迟延，QPS,TPS等相关metrics。

## 技术方案

* 流程图 
    ![](https://yu199195.github.io/images/soul/soul-metrics.png)

* 异步或者同步的方式，在soul网关里面进行 `metrics` 埋点。

* `prometheus` 服务端通过 http 请求 来 拉取  `metrics`, 再使用 `Grafana ` 展示。

## 插件设置

* 在 `soul-admin`--> 插件管理-> monitor ,设置为开启。

* 在 monitor 插件中新增以下配置
```yaml
{"metricsName":"prometheus","host":"localhost","port":"9191","async":"true"}

# port : 为暴露给 prometheus服务来拉取的端口
# host : 不填写则为soul网关的host.
# async :"true" 为异步埋点， false 为同步埋点
```

* 如果用户不使用，则在 `soul-admin` 后台把此插件停用.

## 插件使用

* 在网关的 pom.xml 文件中添加 `monitor` 的支持。
```xml
  <!-- soul monitor plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-monitor</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul monitor plugin end-->
``` 
* 选择器和规则，请详细看 : [选择器规则](selector.md)。
   
   * 只有当匹配的url，才会进行url请求埋点。

## metrics信息

* 所有的JVM，线程，内存，等相关信息都会埋点，可以在 `Granfana ` 面板中，新增一个 JVM 模块，则会完全展示 具体请看 ： https://github.com/prometheus/jmx_exporter

* 另外还有如下自定义的 `metrics` 

 | 名称                      | 类型                  |标签名称       | 说明                  |
 |:------------------------ |:--------------------- |:-------------|:-------------------- |
 |request_total             |Counter                | 无           |收集Soul网关所有的请求 |
 |http_request_total        |Counter                 | path,type    |收集monitor插件匹配的请求| 
 
## 收集 metrics

 * 用户自己搭建`Prometheus` 服务，在 prometheus.yml 文件中新增如下配置:
 
 ```yaml
 scrape_configs:
   # The job name is added as a label `job=<job_name>` to any timeseries scraped from this config.
   - job_name: 'shardingSphere-proxy'
     # metrics_path defaults to '/metrics'
     # scheme defaults to 'http'.
     static_configs:
     - targets: ['localhost:9191']
 ```
 
 ## 面板展示
 
 推荐使用 `Granfana`，用户可以自定义查询来个性化显示面板盘，后续我会提供默认的面板盘配置。
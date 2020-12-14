---
title: springCloud接入soul网关
keywords: soul
description: springCloud接入soul网关
---

## 说明

* 此篇文章是教你如何将springCloud接口，快速接入到soul网关。

* 请在 soul-admin 后台将 `springCloud` 插件设置为开启。

* 接入前，请正确的启动 `soul-admin` , 以及[搭建环境](setup.md) Ok。

## 引入网关 springCloud的插件支持

* 在网关的 pom.xml 文件中引入如下依赖。

```xml
  <!--soul springCloud plugin start-->
  <dependency>
       <groupId>org.dromara</groupId>
       <artifactId>soul-spring-boot-starter-plugin-springcloud</artifactId>
       <version>2.2.0</version>
  </dependency>
   <!--soul springCloud plugin end-->

   <dependency>
        <groupId>org.springframework.cloud</groupId>
        <artifactId>spring-cloud-commons</artifactId>
        <version>2.2.0.RELEASE</version>
   </dependency> 
   <dependency>
        <groupId>org.springframework.cloud</groupId>
        <artifactId>spring-cloud-starter-netflix-ribbon</artifactId>
        <version>2.2.0.RELEASE</version>
   </dependency>
```

* 如果你使用 `eureka` 作为 springCloud的注册中心

  * 新增如下依赖：
  
 ```xml
   <dependency>
        <groupId>org.springframework.cloud</groupId>
        <artifactId>spring-cloud-starter-netflix-eureka-client</artifactId>
        <version>2.2.0.RELEASE</version>
   </dependency>
   ```
  
   * 在网关的yml文件中 新增如下配置:
   
 ```yaml
    eureka:
      client:
        serviceUrl:
          defaultZone: http://localhost:8761/eureka/ # 你的eureka地址
      instance:
        prefer-ip-address: true
   ```

* 如果你使用 `nacos` 作为 springCloud的注册中心

  * 新增如下依赖：
  
 ```xml
  <dependency>
        <groupId>com.alibaba.cloud</groupId>
        <artifactId>spring-cloud-starter-alibaba-nacos-discovery</artifactId>
        <version>2.1.0.RELEASE</version>
  </dependency>
   ```
  
   * 在网关的yml文件中 新增如下配置:
   
 ```yaml
   spring:
      cloud:
        nacos:
          discovery:
             server-addr: 127.0.0.1:8848 # 你的nacos地址
   ```

* 重启你的网关服务。

## SpringCloud服务接入网关。

* 在你提供服务的项目中,引入如下依赖：

```xml
 <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-client-springcloud</artifactId>
      <version>2.2.0</version>
 </dependency>
```

* 在你的yml文件中新增如下配置:

```yaml
soul:
  springcloud:
    admin-url: http://localhost:9095
    context-path: /springcloud
    appName: http
# adminUrl: 为你启动的soul-admin 项目的ip + 端口，注意要加http://
# contextPath: 为你的这个mvc项目在soul网关的路由前缀，这个你应该懂意思把？ 比如/order ，/product 等等，网关会根据你的这个前缀来进行路由.
# appName：你的应用名称，不配置的话，会默认取 `spring.application.name` 的值
```


* 在你的 `controller`的接口上加上 `@SoulSpringCloudClient` 注解

 * 你可以把注解加到 `Controller` 类上面, 里面的path属性则为前缀，如果含有 `/**` 代表你的整个接口需要被网关代理
  
   * 举列子 （1）： 代表 `/test/payment`, `/test/findByUserId` 都会被网关代理。
   
 ```java
  @RestController
  @RequestMapping("/test")
  @SoulSpringCloudClient(path = "/test/**")
  public class HttpTestController {
      
      @PostMapping("/payment")
      public UserDTO post(@RequestBody final UserDTO userDTO) {
          return userDTO;
      }
      
      @GetMapping("/findByUserId")
      public UserDTO findByUserId(@RequestParam("userId") final String userId) {
          UserDTO userDTO = new UserDTO();
          userDTO.setUserId(userId);
          userDTO.setUserName("hello world");
          return userDTO;
      }    
   }
```
  
   * 举列子 （2）： 代表 `/order/save`,会被网关代理,而`/order/findById` 则不会。
  
 ```java
  @RestController
  @RequestMapping("/order")
  @SoulSpringCloudClient(path = "/order")
  public class OrderController {
  
      @PostMapping("/save")
      @SoulSpringMvcClient(path = "/save")
      public OrderDTO save(@RequestBody final OrderDTO orderDTO) {
          orderDTO.setName("hello world save order");
          return orderDTO;
      }
 
      @GetMapping("/findById")
      public OrderDTO findById(@RequestParam("id") final String id) {
          OrderDTO orderDTO = new OrderDTO();
          orderDTO.setId(id);
          orderDTO.setName("hello world findById");
          return orderDTO;
      }
  }
```

  
* 启动你的服务，如果输出以下日志: `http client register success`, 证明你的接口已经被注册到soul网关。  
  
## 插件设置

* 在 `soul-admin` 插件管理中，把 springCloud插件设置为开启。

## 用户请求

* 说白了，你之前怎么请求就怎么请求，没有很大的变动，变动的地方有2点。

* 第一点，你之前请求的域名是你自己的服务，现在要换成网关的域名 （这个你听的懂？）

* 第二点，soul网关需要有一个路由前缀，这个路由前缀就是你接入项目进行配置 `contextPath` ,如果熟的话，可以自由在 `soul-admin` 中的 springCloud插件进行自由更改.
 
```yaml

# 比如你有一个 order服务 它有一个接口，请求路径 http://localhost:8080/test/save

# 现在就需要换成：http://localhost:9195/order/test/save

# 其中 localhost:9195 为网关的ip端口，默认端口是9195 ，/order 是你接入网关配置的 contextPath

# 其他参数，请求方式不变。

# 我讲到这里还不懂？ 请加群问吧

```
* 然后你就可以进行访问了，如此的方便与简单。
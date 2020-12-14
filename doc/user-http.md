---
title: http用户
keywords: soul
description: http用户
---

## 说明

* 本文旨在帮助http用户。

* soul网关使用 divide 插件来处理http请求。请求在soul-admin后台开启它。

* 接入前，请正确的启动 `soul-admin` , 以及 [搭建环境](setup.md) OK。

## 引入网关对http的代理插件

* 在网关的 `pom.xml` 文件中增加如下依赖：
```xml
  <!--if you use http proxy start this-->
   <dependency>
       <groupId>org.dromara</groupId>
       <artifactId>soul-spring-boot-starter-plugin-divide</artifactId>
       <version>2.2.0</version>
   </dependency>

   <dependency>
       <groupId>org.dromara</groupId>
       <artifactId>soul-spring-boot-starter-plugin-httpclient</artifactId>
       <version>2.2.0</version>
   </dependency>
```

* 当然是要重新启动网关。

## Http请求接入网关（springMvc体系用户）

* 首先要确保在 `soul-admin` 后台 divide插件是否开启。

##### Soul-Client接入方式。 （此方式针对SpringMvc,SpringBoot用户）

* SpringBoot用户
  
   * 在你的真实服务的 `pom.xml` 新增如下依赖: 
```xml
     <dependency>
         <groupId>org.dromara</groupId>
         <artifactId>soul-spring-boot-starter-client-springmvc</artifactId>
         <version>2.2.0</version>
     </dependency>
 ```
   * 在yml中新增如下配置 ：  
```yaml
   soul:
     http:
       adminUrl: http://localhost:9095
       port: 你本项目的启动端口
       contextPath: /http
       appName: http
       full: false  
   # adminUrl: 为你启动的soul-admin 项目的ip + 端口，注意要加http://
   # port: 你本项目的启动端口
   # contextPath: 为你的这个mvc项目在soul网关的路由前缀，这个你应该懂意思把？ 比如/order ，/product 等等，网关会根据你的这个前缀来进行路由.
   # appName：你的应用名称，不配置的话，会默认取 `spring.application.name` 的值
   # full: 设置true 代表代理你的整个服务，false表示代理你其中某几个controller
   ``` 
 * SpringMvc用户 
   * 在你的真实服务的 `pom.xml` 新增如下依赖: 
```xml
       <dependency>
           <groupId>org.dromara</groupId>
           <artifactId>soul-client-springmvc</artifactId>
           <version>2.2.0</version>
       </dependency>
 ```     
  * 在你的 bean定义的xml文件中新增如下 ：   
 ```xml
    <bean id ="springMvcClientBeanPostProcessor" ,class ="org.dromara.soul.client.springmvc.init.SpringMvcClientBeanPostProcessor">
         <constructor-arg  ref="soulSpringMvcConfig"/>
    </bean>
    
    <bean id="soulSpringMvcConfig", class="org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig">
         <property name="adminUrl" value="http://localhost:9095"/>
         <property name="port" value="你的端口"/>
         <property name="contextPath" value="/你的contextPath"/>
         <property name="appName" value="你的名字"/>
         <property name="full" value="false"/>
    </bean>
   ``` 
* 在你的 `controller` 的接口上加上 `@SoulSpringMvcClient` 注解。
  
   * 你可以把注解加到 `Controller` 类上面, 里面的path属性则为前缀，如果含有 `/**` 代表你的整个接口需要被网关代理。
  
   * 举列子 （1）： 代表 `/test/payment`, `/test/findByUserId` 都会被网关代理。
   
 ```java
  @RestController
  @RequestMapping("/test")
  @SoulSpringMvcClient(path = "/test/**")
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
  @SoulSpringMvcClient(path = "/order")
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

* 启动你的项目，你的接口接入到了网关。

## Http请求接入网关（其他语言，非springMvc体系）

* 首先在 `soul-admin` 找到 divide插件，进行选择器，和规则的添加，进行流量的匹配筛选。

* 如果不懂怎么配置，请看选择，规则介绍 [选择器规则介绍](selector.md)。

* 您也可以自定义开发属于你的 http-client，参考 [多语言Http客户端开发](dev-client.md)。

## 用户请求

* 说白了，你之前怎么请求就怎么请求，没有很大的变动，变动的地方有2点。

* 第一点，你之前请求的域名是你自己的服务，现在要换成网关的域名 （这个你听的懂？）

* 第二点，soul网关需要有一个路由前缀，这个路由前缀就是你接入项目进行配置 `contextPath` ,如果熟的话，可以自由在 `soul-admin` 中的divide插件进行自由更改.
 
```yaml

# 比如你有一个 order服务 它有一个接口，请求路径 http://localhost:8080/test/save

# 现在就需要换成：http://localhost:9195/order/test/save

# 其中 localhost:9195 为网关的ip端口，默认端口是9195 ，/order 是你接入网关配置的 contextPath

# 其他参数，请求方式不变。

# 我讲到这里还不懂？ 请加群问吧

```
* 然后你就可以进行访问了，如此的方便与简单。

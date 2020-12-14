---
title: dubbo接入soul网关
keywords: soul
description: dubbo接入soul网关
---


## 说明

* 此篇文章是dubbo用户使用dubbo插件支持，以及自己的dubbo服务接入soul网关的教程。

* 支持 alibaba dubbo（< 2.7.x） 以及 apache dubbo (>=2.7.x)。

* 接入前，请正确的启动 `soul-admin` , 以及[搭建环境](setup.md) Ok。


## 引入网关对dubbo支持的插件


* 在网关的 `pom.xml` 文件中增加如下依赖：

  * alibaba dubbo 用户, dubbo版本换成你的，注册中心的jar包换成你的，一下是参考。
  
 ```xml
  
          <!--soul alibaba dubbo plugin start-->
          <dependency>
              <groupId>org.dromara</groupId>
              <artifactId>soul-spring-boot-starter-plugin-alibaba-dubbo</artifactId>
              <version>2.2.0</version>
          </dependency>
           <!-- soul  alibaba dubbo plugin end-->
          <dependency>
              <groupId>com.alibaba</groupId>
              <artifactId>dubbo</artifactId>
              <version>2.6.5</version>
          </dependency>
          <dependency>
              <groupId>org.apache.curator</groupId>
              <artifactId>curator-client</artifactId>
              <version>4.0.1</version>
          </dependency>
          <dependency>
              <groupId>org.apache.curator</groupId>
              <artifactId>curator-framework</artifactId>
              <version>4.0.1</version>
          </dependency>
          <dependency>
              <groupId>org.apache.curator</groupId>
              <artifactId>curator-recipes</artifactId>
              <version>4.0.1</version>
          </dependency>
       
  ```
  
  * apache dubbo 用户，dubbo版本换成你的，使用什么注册中心换成你的,以下是参考,使用什么注册中心，就引入啥。
  
  ```xml
  
           <!--soul apache dubbo plugin start-->
           <dependency>
               <groupId>org.dromara</groupId>
               <artifactId>soul-spring-boot-starter-plugin-apache-dubbo</artifactId>
               <version>2.2.0</version>
           </dependency>
           <!--soul apache dubbo plugin end-->
  
           <dependency>
               <groupId>org.apache.dubbo</groupId>
               <artifactId>dubbo</artifactId>
               <version>2.7.5</version>
           </dependency>
            <!-- Dubbo Nacos registry dependency start -->
           <dependency>
               <groupId>org.apache.dubbo</groupId>
               <artifactId>dubbo-registry-nacos</artifactId>
               <version>2.7.5</version>
           </dependency>
           <dependency>
               <groupId>com.alibaba.nacos</groupId>
               <artifactId>nacos-client</artifactId>
               <version>1.1.4</version>
           </dependency>
           <!-- Dubbo Nacos registry dependency  end-->
  
           <!-- Dubbo zookeeper registry dependency start-->
           <dependency>
               <groupId>org.apache.curator</groupId>
               <artifactId>curator-client</artifactId>
               <version>4.0.1</version>
           </dependency>
           <dependency>
               <groupId>org.apache.curator</groupId>
               <artifactId>curator-framework</artifactId>
               <version>4.0.1</version>
           </dependency>
           <dependency>
               <groupId>org.apache.curator</groupId>
               <artifactId>curator-recipes</artifactId>
               <version>4.0.1</version>
           </dependency>
           <!-- Dubbo zookeeper registry dependency end -->
  ```

* 重启网关服务。

## dubbo服务接入网关,可以参考 : [soul-test-dubbo](https://github.com/Dromara/soul/tree/master/soul-test/soul-test-dubbo)

 * alibaba dubbo 用户
   
    * springboot
      
       * 引入以下依赖
 ```xml
        <dependency>
             <groupId>org.dromara</groupId>
             <artifactId>soul-spring-boot-starter-client-alibaba-dubbo</artifactId>
             <version>2.2.0</version>
        </dependency>
 ```
      
  * 在你的yml文件中新增如下配置 ：
  
   ```yaml
      soul:
        dubbo:
          adminUrl: http://localhost:9095
          contextPath: /dubbo
          appName: dubbo    
         # adminUrl: 为你启动的soul-admin 项目的ip + 端口，注意要加 http://
         # contextPath: 为你的这个项目在soul网关的路由前缀，这个你应该懂意思把？ 比如/order ，/product 等等，网关会根据你的这个前缀来进行路由.
         # appName：你的应用名称，不配置的话，会默认取 dubbo配置中application 中的名称
  ```    
    
* spring
     
   * 引入以下依赖 ：
 ```xml
       <dependency>
           <groupId>org.dromara</groupId>
           <artifactId>soul-client-alibaba-dubbo</artifactId>
           <version>2.2.0</version>
        </dependency>
   ```
   * 在你的 bean定义的xml文件中新增如下 ：      
  ```xml
          <bean id ="alibabaDubboServiceBeanPostProcessor" ,class ="org.dromara.soul.client.alibaba.dubbo.AlibabaDubboServiceBeanPostProcessor">
               <constructor-arg  ref="dubboConfig"/>
          </bean>
          
          <bean id="dubboConfig", class="org.dromara.soul.client.dubbo.common.config.DubboConfig">
               <property name="adminUrl" value="http://localhost:9095"/>     
               <property name="contextPath" value="/你的contextPath"/>
               <property name="appName" value="你的名字"/>
          </bean>
   ``` 
   
* apache dubbo 用户
   
  * springboot
      
     * 引入以下依赖
       
 ```xml
        <dependency>
             <groupId>org.dromara</groupId>
             <artifactId>soul-spring-boot-starter-client-apache-dubbo</artifactId>
             <version>2.2.0</version>
        </dependency>
   ```
      
 * 在你的yml文件中新增如下配置 ：
     
  ```yaml
      soul:
        dubbo:
          adminUrl: http://localhost:9095
          contextPath: /dubbo
          appName: dubbo
      
         # adminUrl: 为你启动的soul-admin 项目的ip + 端口，注意要加 http://
         # contextPath: 为你的这个项目在soul网关的路由前缀，这个你应该懂意思把？ 比如/order ，/product 等等，网关会根据你的这个前缀来进行路由.
         # appName：你的应用名称，不配置的话，会默认取 dubbo配置中application 中的名称
  ```
   
   * spring
      
     * 引入以下依赖 ：
       
     ```xml
       <dependency>
           <groupId>org.dromara</groupId>
           <artifactId>soul-client-apache-dubbo</artifactId>
           <version>2.2.0</version>
        </dependency>
       ```

* 在你的 bean定义的xml文件中新增如下 ：
       
 ```xml
          <bean id ="apacheDubboServiceBeanPostProcessor" ,class ="org.dromara.soul.client.apache.dubbo.ApacheDubboServiceBeanPostProcessor">
               <constructor-arg  ref="dubboConfig"/>
          </bean>
          
          <bean id="dubboConfig", class="org.dromara.soul.client.dubbo.common.config.DubboConfig">
               <property name="adminUrl" value="http://localhost:9095"/>     
               <property name="contextPath" value="/你的contextPath"/>
               <property name="appName" value="你的名字"/>
          </bean>
   ``` 

## dubbo 插件设置

* 首先在 `soul-admin` 插件管理中，把`dubbo` 插件设置为开启。

* 其次在 `dubbo ` 插件中配置你的注册地址,或者其他注册中心的地址.

```yaml
{"register":"zookeeper://localhost:2181"}   or {"register":"nacos://localhost:8848"}

```

## 接口注册到网关

* 你dubbo服务实现类的，方法上加上 `@SoulDubboClient` 注解，表示该接口方法注册到网关。

* 启动你的提供者,输出日志 `dubbo client register success ` 大功告成，你的dubbo接口已经发布到 soul网关.如果还有不懂的，可以参考 `soul-test-dubbo`项目.

## dubbo用户请求以及参数说明

* 说白了，就是通过http的方式来请求你的dubbo服务

* soul网关需要有一个路由前缀，这个路由前缀就是你接入项目进行配置 `contextPath`
 
```yaml
# 比如你有一个 order服务 它有一个接口，它的注册路径 /order/test/save

# 现在就是通过 post方式请求网关：http://localhost:9195/order/test/save

# 其中 localhost:9195 为网关的ip端口，默认端口是9195 ，/order 是你dubbo接入网关配置的 contextPath

```

* 参数传递：
   
   * 通过 http post 方式访问网关，通过body，json类型传递。
   
   * 更多参数类型传递，可以参考 [soul-test-dubbo](https://github.com/Dromara/soul/tree/master/soul-test/soul-test-dubbo) 中的接口定义，以及参数传递方式。

* 单个java bean参数类型 （默认）

* 多参数类型支持 ,在网关的yaml 配置中新增如下配置：

```yaml
soul :
    dubbo :
      parameter: multi
```

* 自定义实现多参数支持:

  * 在你搭建的网关项目中，新增一个类 A，实现 `org.dromara.soul.web.dubbo.DubboParamResolveService`。
  
 ```java
   public interface DubboParamResolveService {
   
       /**
        * Build parameter pair.
        * this is Resolve http body to get dubbo param.
        *
        * @param body           the body
        * @param parameterTypes the parameter types
        * @return the pair
        */
       Pair<String[], Object[]> buildParameter(String body, String parameterTypes);
   }
  ```
  
  * `body`为http中body传的json字符串.
  
  *  `parameterTypes`: 匹配到的方法参数类型列表，如果有多个,则使用`,`分割。
  
  *  Pair中，left为参数类型，right为参数值，这是dubbo泛化调用的标准
  
  * 把你的类注册成Spring的bean，覆盖默认的实现。
  
 ```java
  @Bean
  public DubboParamResolveService A() {
          return new A();
  }
  ```
  
## 大白话讲解如果通过http --> 网关--> dubbo provider

* 说白了，就是把http请求，转成dubbo协议，内部使用dubbo泛化来进行调用。

* 首先你要回想下，你的dubbo服务在接入网关的时候，是不是加了个 `@SoulDubboClient` 注解，里面是不是有个path字段来指定你请求的路径？

* 你是不是还在yml中配置了一个 `contextPath`?

* 如果您还记得，那我们就开始。

* 假如你有一个这样的方法, contextPath 配置的是 `/dubbo`

```java
    @Override
    @@SoulDubboClient(path = "/insert", desc = "插入一条数据")
    public DubboTest insert(final DubboTest dubboTest) {
        return dubboTest;
    }

```

* 那么我们请求的路径为: http://localhost:9195/dubbo/insert ,再说一下，localhost:9195是网关的域名，如果你更改了，这里也要改。

* 那么请求参数呢？ `DubboTest` 是一个javabean对象，有2个字段，id与name ，那么我们通过body中传递这个对象的json数据就好。

```
{"id":"1234","name":"XIAO5y"}

```

* 如果你的接口中,没有参数，那么body传值为:

```
{}

```

* 如果你的接口有很多个参数? 往上看一点，有介绍。
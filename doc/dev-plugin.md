---
title: 插件扩展
keywords: soul
description: 插件扩展
---

## 说明

* 插件是 soul 网关的核心执行者，每个插件在开启的情况下，都会对匹配的流量，进行自己的处理。

* 在soul 网关里面，插件其实分为2 类：
  
   * 一类是单一职责的调用链，不能对流量进行自定义的筛选。
   
   * 另一类，能对匹配的流量，执行自己的职责调用链。

* 用户可以参考 soul-plugin 模块，新增自己的插件处理，如果有好的公用插件，请把代码提交上来。

## 单一职责插件

* 引入如下依赖 :

```
 <dependency>
        <groupId>org.dromara</groupId>
        <artifactId>soul-plugin-api</artifactId>
        <version>2.2.0</version>
  </dependency>
```

* 用户新增一个类 A,直接实现 `org.dromara.soul.plugin.api.SoulPlugin`

```java
public interface SoulPlugin {
    
    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link SoulPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    Mono<Void> execute(ServerWebExchange exchange, SoulPluginChain chain);
    
    /**
     * return plugin order .
     * This attribute To determine the plugin execution order in the same type plugin.
     *
     * @return int order
     */
    int getOrder();
    
    /**
     * acquire plugin name.
     * this is plugin name define you must Provide the right name.
     * if you impl AbstractSoulPlugin this attribute not use.
     *
     * @return plugin name.
     */
    default String named() {
        return "";
    }
    
    /**
     * plugin is execute.
     * if return true this plugin can not execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    default Boolean skip(ServerWebExchange exchange) {
        return false;
    }
}

```
* 接口方法详细说明
       
   * `execute()` 方法为核心的执行方法，用户可以在里面自由的实现自己想要的功能。
   
   * `getOrder()` 指定插件的排序。
   
   * `named()` 指定插件的名称。
   
   * `skip()` 在特定的条件下，该插件是否被跳过。
   

* 注册成Spring的bean，参考如下,或者直接在实现类上加 `@Component` 注解。

```java
    @Bean
    public SoulPlugin a() {
        return new A();
    }
```
 

## 匹配流量处理插件

* 引入如下依赖 :
```xml
 <dependency>
        <groupId>org.dromara</groupId>
        <artifactId>soul-plugin-base</artifactId>
        <version>2.2.0</version>
  </dependency>
```
*  新增一个类A，继承 `org.dromara.soul.plugin.base.AbstractSoulPlugin`

* 以下是参考 ：

```java
/**
 * This is your custom plugin.
 * He is running in after before plugin, implement your own functionality.
 * extends AbstractSoulPlugin so you must user soul-admin And add related plug-in development.
 *
 * @author xiaoyu(Myth)
 */
public class CustomPlugin extends AbstractSoulPlugin {
    
    /**
     * return plugin order .
     * The same plugin he executes in the same order.
     *
     * @return int
     */
    @Override
    public int getOrder() {
        return 0;
    }

    /**
     * acquire plugin name.
     * return you custom plugin name.
     * It must be the same name as the plug-in you added in the admin background.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return "soul";
    }

    /**
     * plugin is execute.
     * Do I need to skip.
     * if you need skip return true.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        return false;
    }

    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, SoulPluginChain chain, SelectorZkDTO selector, RuleZkDTO rule) {
        LOGGER.debug(".......... function plugin start..............");
        /*
         * Processing after your selector matches the rule.
         * rule.getHandle() is you Customize the json string to be processed.
         * for this example.
         * Convert your custom json string pass to an entity class.
         */
        final String ruleHandle = rule.getHandle();

        final Test test = GsonUtils.getInstance().fromJson(ruleHandle, Test.class);

        /*
         * Then do your own business processing.
         * The last execution  chain.execute(exchange).
         * Let it continue on the chain until the end.
         */
        System.out.println(test.toString());
        return chain.execute(exchange);
    }
}

```

* 详细讲解 ：

   * 继承该类的插件，插件会进行选择器规则匹配，那如何来设置呢？

   * 首先在 `soul-admin` 后台 -->插件管理中，新增一个插件，注意 名称与 你自定义插件的 `named（）` 方法要一致。

   * 重新登陆  `soul-admin` 后台 ，你会发现在插件列表就多了一个你刚新增的插件，你就可以进行选择器规则匹配

   * 在规则中，有个 `handler` 字段，是你自定义处理数据，在 `doExecute()` 方法中，通过 ` final String ruleHandle = rule.getHandle();` 获取，然后进行你的操作。

* 注册成Spring的bean，参考如下,或者直接在实现类上加 `@Component` 注解。

```java
    @Bean
    public SoulPlugin a() {
        return new A();
    }
```

## 订阅你的插件数据，进行自定义的处理

* 新增一个类A，实现 `org.dromara.soul.plugin.base.handler.PluginDataHandler`

```java
public interface PluginDataHandler {
    
    /**
     * Handler plugin.
     *
     * @param pluginData the plugin data
     */
    default void handlerPlugin(PluginData pluginData) {
    }
    
    /**
     * Remove plugin.
     *
     * @param pluginData the plugin data
     */
    default void removePlugin(PluginData pluginData) {
    }
    
    /**
     * Handler selector.
     *
     * @param selectorData the selector data
     */
    default void handlerSelector(SelectorData selectorData) {
    }
    
    /**
     * Remove selector.
     *
     * @param selectorData the selector data
     */
    default void removeSelector(SelectorData selectorData) {
    }
    
    /**
     * Handler rule.
     *
     * @param ruleData the rule data
     */
    default void handlerRule(RuleData ruleData) {
    }
    
    /**
     * Remove rule.
     *
     * @param ruleData the rule data
     */
    default void removeRule(RuleData ruleData) {
    }
    
    /**
     * Plugin named string.
     *
     * @return the string
     */
    String pluginNamed();
    
}
```

* 注意 `pluginNamed()` 要和你自定义的插件名称相同。

* 注册成Spring的bean，参考如下,或者直接在实现类上加 `@Component` 注解。
```java
    @Bean
    public PluginDataHandler a() {
        return new A();
    }
```
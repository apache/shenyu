---
title: sign插件
keywords: sign
description: sign插件
---

## 说明

* sign插件是 soul网关自带的，用来对请求进行签名认证的插件。


## 插件设置

* 在 `soul-admin` -> 插件管理中 --> `sign`插件设置为开启。

## 插件使用

* 在网关的 pom.xml 文件中添加 `sign` 的支持。

```xml
  <!-- soul sign plugin start-->
  <dependency>
      <groupId>org.dromara</groupId>
      <artifactId>soul-spring-boot-starter-plugin-sign</artifactId>
      <version>2.2.0</version>
  </dependency>
  <!-- soul sign plugin end-->
``` 

* 选择器和规则，请详细看 : [选择器规则](selector.md)。

  * 只有匹配的请求，才会进行签名认证。


## 新增 AK/SK

* 在soul-admin --> 认证管理中，点击新增，新增一条 AK/SK。


## 网关技术实现
 
 * 采用Ak/SK鉴权技术方案。
 * 采用鉴权插件，责任链的模式的模式来完成。
 * 当鉴权插件开启，并配置所有接口鉴权时候生效。
 
 
 ## 鉴权使用指南
 
 * 第一步：AK/SK由网关来进行分配. 比如分配给你的AK为: `1TEST123456781`  	SK为：`506EEB535CF740D7A755CB4B9F4A1536` 
 
 * 第一步：确定好你要访问的网关路径 比如 `/api/service/abc`
 
 * 第三步:构造参数（以下是通用参数）
 
| 字段        | 值    |  描述  |
| --------   | -----:  | :----: |
| timestamp  |  当前时间戳(String类型)   |  当前时间的毫秒数（网关会过滤掉5分钟之前的请求）    |
| path       | /api/service/abc  | 就是你需要访问的接口路径(根据你访问网关接口自己变更) |
| version       | 1.0.0  | 目前定位1.0.0 写死，String类型 |

 对上述2个字段进行key的自然排序，然后进行字段与字段值拼接最后再拼接上SK,代码示例。
 

第一步:首先构造一个Map。
```java

   Map<String, String> map = Maps.newHashMapWithExpectedSize(2);
   //timestamp为毫秒数的字符串形式 String.valueOf(LocalDateTime.now().toInstant(ZoneOffset.of("+8")).toEpochMilli()) 
   map.put("timestamp","1571711067186");  //值应该为毫秒数的字符串形式 
   map.put("path", "/api/service/abc");
   map.put("version", "1.0.0");
```

第二步:进行Key的自然排序，然后Key，Value值拼接最后再拼接分配给你的Sk。
```java
List<String> storedKeys = Arrays.stream(map.keySet()
                .toArray(new String[]{}))
                .sorted(Comparator.naturalOrder())
                .collect(Collectors.toList());
final String sign = storedKeys.stream()
                .map(key -> String.join("", key, params.get(key)))
                .collect(Collectors.joining()).trim()
                .concat("506EEB535CF740D7A755CB4B9F4A1536");
```
* 你得到的sign值应该为:path/api/service/abctimestamp1571711067186version1.0.0506EEB535CF740D7A755CB4B9F4A1536

第三步:进行Md5加密后转成大写。
```java
DigestUtils.md5DigestAsHex(sign.getBytes()).toUpperCase()
```

* 最后得到的值为:A021BF82BE342668B78CD9ADE593D683
 
## 请求网关

* 假如你访问的路径为 :/api/service/abc。

* 访问地址 ：http:网关的域名/api/service/abc。

* 设置`header`头，`header`头参数为：

| 字段        | 值    |  描述  |
| --------   | -----:  | :----: |
| timestamp  |   `1571711067186`  |  上述你进行签名的时候使用的时间值   |
| appKey     | `1TEST123456781`  | 分配给你的Ak值 |
| sign       | `A90E66763793BDBC817CF3B52AAAC041`  | 上述得到的签名值 |
| version       | `1.0.0`  | 写死，就为这个值 |

* 签名插件会默认过滤5分钟之后的请求

## 如果认证不通过会返回 code 为401 message可能会有变动。

```json
"code":401,"message":"sign is not pass,Please check you sign algorithm!","data":null}
```

## 签名认证算法扩展

* 请参考开发者文档中的 [扩展签名算法](dev-sign.md)。
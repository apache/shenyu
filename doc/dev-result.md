---
title: 自定义网关返回数据格式
keywords: soul
description: 自定义网关返回数据格式
---

## 说明

* 本文是说明,基于soul网关，返回自定义的数据个数。

* 网关需要统一的返回格式,而每个公司都有自己定义的一套，所以需要对次进行扩展。


### 默认实现

* 默认的实现为 `org.dromara.soul.plugin.api.result.DefaultSoulResult`

* 返回的数据格式如下：

```java
public class SoulDefaultEntity implements Serializable {

    private static final long serialVersionUID = -2792556188993845048L;

    private Integer code;

    private String message;

    private Object data;

}
```

* 返回的json 格式如下:
```json
{
    "code": -100, //返回码,
    "message": "您的参数错误,请检查相关文档!", //提示字段
    "data": null  // 具体的数据
}
```

## 扩展

*  新增一个类 A 实现 `org.dromara.soul.plugin.api.result.SoulResult`

```java
 public interface SoulResult<T> {
 
     /**
      * Success t.
      *
      * @param code    the code
      * @param message the message
      * @param object  the object
      * @return the t
      */
     T success(int code, String message, Object object);

     /**
      * Error t.
      *
      * @param code    the code
      * @param message the message
      * @param object  the object
      * @return the t
      */
     T error(int code, String message, Object object);
 }

```

* 其他 泛型 T 为你自定义的数据格式，返回它就好


* 把你新增的实现类注册成为spring的bean,如下

```java
    @Bean
    public SoulResult a() {
          return new A();
    }
```




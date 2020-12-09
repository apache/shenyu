---
title: 元数据概念设计
keywords: soul
description: 元数据概念设计
---

## 说明

* 本篇主要讲解在soul网关中元数据的概念，设计，以及如何对接。

## 技术方案

* 在数据库中，新增了一张表，然后通过数据同步的方案，会把这张表的数据同步到网关JVM内存。

* 表结构如下:
```
CREATE TABLE  IF NOT EXISTS `meta_data` (
  `id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'id',
  `app_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '应用名称',
  `path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路径,不能重复',
  `path_desc` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '路径描述',
  `rpc_type` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'rpc类型',
  `service_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '服务名称',
  `method_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '方法名称',
  `parameter_types` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT '参数类型 多给参数类型 逗号隔开',
  `rpc_ext` varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NULL DEFAULT NULL COMMENT 'rpc的扩展信息，json格式',
  `date_created` datetime(0) NOT NULL COMMENT '创建时间',
  `date_updated` datetime(0) NOT NULL ON UPDATE CURRENT_TIMESTAMP(0) COMMENT '更新时间',
  `enabled` tinyint(4) NOT NULL DEFAULT 0 COMMENT '启用状态',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci ROW_FORMAT = Dynamic;

```

* 元数据设计，目前最主要的是对dubbo的泛化调用上进行使用。

* 我重点讲一下 `path` 字段，在请求网关的时候，会根据你的path字段来匹配到一条数据，然后进行后续的流程

* 重点讲一下 `rcp_ext`字段,如果是dubbo类型的服务接口，如果服务接口设置了 group,version字段的时候，会存在这个字段.

  * dubbo 类型 字段结构是 如果，那么存储的就是json格式的字符串..
  
  ```
   public static class RpcExt {
          
          private String group;
          
          private String version;
          
          private String loadbalance;
          
          private Integer retries;
          
          private Integer timeout;
  
          private String url;
  
      }
  ```

## 元数据存储

* 每个dubbo接口方法，对应一条元数据。

* springcloud协议，只会存储一条数据， path为 `/contextPath/**`。

* http服务，则不会有任何数据。



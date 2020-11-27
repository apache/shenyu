---
title: 多语言http客户端
keywords: soul
description: 多语言http客户端
---

## 说明

* 本文主要讲解其他语言的http服务如何接入网关。

* 如何自定义开发 soul-http-client


## 自定义开发

* 请求方式: `POST`

* 请求路径

    * `http://soul-admin/soul-client/springmvc-register`  soul-admin, 表示为 admin的 ip + port

* 请求参数

* soul网关默认的需要参数,通过body里面传，json类型。
```json
{
	"appName": "xxx", //应用名称 必填
	"context": "/xxx", //请求前缀 必填
	"path": "xxx", //路径需要唯一 必填
	"rpcType": "http", //rpc类型  必填
	"host": "xxx", //服务host 必填
	"port": xxx, //服务端口 必填
	"ruleName": "xxx", //可以同path一样  必填
	"enabled": "true"
}
```






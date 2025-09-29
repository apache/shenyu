# MCP Tool 配置示例大全

本文档基于 **shenyu-examples-http** 项目中的真实接口，提供了 Shenyu MCP Server Plugin 的各种工具配置示例，涵盖不同的 HTTP 请求方法、参数类型和配置方式。

## 1. 简单 GET 请求示例

### 1.1 无参数 GET 请求
```json
{
  "name": "hello",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/hello\",\"method\":\"GET\",\"headers\":[],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "Shenyu 客户端问候接口"
}
```

### 1.2 带参数的 GET 请求
```json
{
  "name": "hi",
  "parameters": [
    {
      "name": "name",
      "type": "string",
      "description": "用户名",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/hi\",\"method\":\"GET\",\"headers\":[],\"timeout\":30000,\"queryParams\":[{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"name\":\"query\"}}",
  "description": "Shenyu 客户端带参数问候接口"
}
```

## 2. 用户相关 GET 请求示例

### 2.1 根据用户ID查找用户
```json
{
  "name": "findByUserId",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "用户ID",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByUserId\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"userId\",\"value\":\"${userId}\"}]},\"argsPosition\":{\"userId\":\"query\"}}",
  "description": "根据用户ID查找用户信息"
}
```

### 2.2 根据用户ID和姓名查找用户
```json
{
  "name": "findByUserIdName",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "用户ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "用户姓名",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByUserIdName\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"userId\",\"value\":\"${userId}\"},{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"userId\":\"query\",\"name\":\"query\"}}",
  "description": "根据用户ID和姓名查找用户"
}
```

### 2.3 分页查找用户
```json
{
  "name": "findByPage",
  "parameters": [
    {
      "name": "keyword",
      "type": "string",
      "description": "搜索关键词",
      "required": false
    },
    {
      "name": "page",
      "type": "integer",
      "description": "页码",
      "required": false
    },
    {
      "name": "pageSize",
      "type": "integer",
      "description": "每页大小",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByPage\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"keyword\",\"value\":\"${keyword}\"},{\"key\":\"page\",\"value\":\"${page}\"},{\"key\":\"pageSize\",\"value\":\"${pageSize}\"}]},\"argsPosition\":{\"keyword\":\"query\",\"page\":\"query\",\"pageSize\":\"query\"}}",
  "description": "分页查找用户"
}
```

### 2.4 通过路径参数获取用户信息
```json
{
  "name": "getUserByPath",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "用户ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "用户姓名",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/path/{{.id}}\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"id\":\"path\",\"name\":\"query\"}}",
  "description": "通过路径参数获取用户信息"
}
```

## 3. POST 请求示例

### 3.1 用户支付接口
```json
{
  "name": "payment",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "用户ID",
      "required": true
    },
    {
      "name": "userName",
      "type": "string",
      "description": "用户姓名",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/payment\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"userId\":\"body\",\"userName\":\"body\"}}",
  "description": "用户支付接口"
}
```

### 3.2 WAF 通过测试
```json
{
  "name": "wafPass",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/waf/pass\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "WAF 通过测试"
}
```

### 3.3 Shenyu 客户端 POST 请求
```json
{
  "name": "postHi",
  "parameters": [
    {
      "name": "name",
      "type": "string",
      "description": "用户名",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/post/hi\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/x-www-form-urlencoded\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"name\":\"body\"}}",
  "description": "Shenyu 客户端 POST 问候接口"
}
```

## 4. PUT 请求示例

### 4.1 更新用户信息
```json
{
  "name": "updateUser",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "用户ID",
      "required": true
    },
    {
      "name": "userId",
      "type": "string",
      "description": "用户ID",
      "required": true
    },
    {
      "name": "userName",
      "type": "string",
      "description": "用户姓名",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/putPathBody/{{.id}}\",\"method\":\"PUT\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"id\":\"path\",\"userId\":\"body\",\"userName\":\"body\"}}",
  "description": "更新用户信息"
}
```

## 5. 订单相关示例

### 5.1 保存订单
```json
{
  "name": "saveOrder",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "订单ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "订单名称",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/save\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"id\":\"body\",\"name\":\"body\"}}",
  "description": "保存订单"
}
```

### 5.2 根据ID查找订单
```json
{
  "name": "findOrderById",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "订单ID",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/findById\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"id\",\"value\":\"${id}\"}]},\"argsPosition\":{\"id\":\"query\"}}",
  "description": "根据ID查找订单"
}
```

### 5.3 RESTful 风格订单查询
```json
{
  "name": "getOrderByPath",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "订单ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "订单名称",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/path/{{.id}}/{{.name}}\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{\"id\":\"path\",\"name\":\"path\"}}",
  "description": "RESTful 风格订单查询"
}
```

## 6. 请求处理示例

### 6.1 请求头测试
```json
{
  "name": "testRequestHeader",
  "parameters": [
    {
      "name": "headerKey1",
      "type": "string",
      "description": "请求头参数",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/header\",\"method\":\"GET\",\"headers\":[{\"key\":\"header_key1\",\"value\":\"{{.headerKey1}}\"}],\"timeout\":30000},\"argsPosition\":{\"headerKey1\":\"query\"}}",
  "description": "请求头测试"
}
```

### 6.2 请求参数测试
```json
{
  "name": "testRequestParameter",
  "parameters": [
    {
      "name": "parameterKey1",
      "type": "string",
      "description": "请求参数",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/parameter\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/x-www-form-urlencoded\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"parameterKey1\":\"body\"}}",
  "description": "请求参数测试"
}
```

### 6.3 Cookie 测试
```json
{
  "name": "testRequestCookie",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "用户ID（Cookie值）",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/cookie\",\"method\":\"GET\",\"headers\":[{\"key\":\"Cookie\",\"value\":\"userId={{.userId}}\"}],\"timeout\":30000},\"argsPosition\":{\"userId\":\"query\"}}",
  "description": "Cookie 测试"
}
```

## 7. 文件上传示例

### 7.1 单文件上传
```json
{
  "name": "uploadSingleFile",
  "parameters": [
    {
      "name": "file",
      "type": "string",
      "description": "文件内容（Base64编码）",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/upload/webFluxSingle\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"multipart/form-data\"}],\"timeout\":60000,\"argsToJsonBody\":true},\"argsPosition\":{\"file\":\"body\"}}",
  "description": "单文件上传"
}
```

### 7.2 多文件上传
```json
{
  "name": "uploadMultipleFiles",
  "parameters": [
    {
      "name": "files",
      "type": "array",
      "description": "多个文件内容（Base64编码数组）",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/upload/webFluxFiles\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"multipart/form-data\"}],\"timeout\":60000,\"argsToJsonBody\":true},\"argsPosition\":{\"files\":\"body\"}}",
  "description": "多文件上传"
}
```

## 8. 高级功能示例

### 8.1 OAuth2 认证测试
```json
{
  "name": "testOAuth2",
  "parameters": [
    {
      "name": "token",
      "type": "string",
      "description": "OAuth2 访问令牌",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/oauth2/test\",\"method\":\"GET\",\"headers\":[{\"key\":\"Authorization\",\"value\":\"Bearer {{.token}}\"}],\"timeout\":30000},\"argsPosition\":{\"token\":\"query\"}}",
  "description": "OAuth2 认证测试"
}
```

### 8.2 缓存测试
```json
{
  "name": "testCache",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/cache\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "缓存测试"
}
```

### 8.3 大对象响应测试
```json
{
  "name": "testBigObject",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/bigObject\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":60000},\"argsPosition\":{}}",
  "description": "大对象响应测试"
}
```

## 参数位置映射说明 (argsPosition)

### 支持的映射位置：

1. **URL 路径参数**: `"path"` - 对应 URL 模板中的 `{{.paramName}}`
2. **查询参数**: `"query"` - 对应 `queryParams` 数组中的参数
3. **请求体**: `"body"` - 配合 `argsToJsonBody: true` 使用

### 示例映射：
```json
{
  "argsPosition": {
    "id": "path",                        // 路径参数 /test/path/{{.id}}
    "userId": "query",                   // 查询参数 queryParams: [{"key": "userId", "value": "${userId}"}]
    "userName": "body"                   // JSON Body (需设置 argsToJsonBody: true)
  }
}
```

### 配置结构说明：

#### requestTemplate 配置项：
- `url`: URL 模板，路径参数支持 `{{.paramName}}` 占位符
- `method`: HTTP 方法 (GET, POST, PUT, DELETE)
- `headers`: 请求头数组，每个元素包含 `key` 和 `value`
- `queryParams`: 查询参数数组，每个元素包含 `key` 和 `value`
- `timeout`: 超时时间（毫秒）
- `argsToJsonBody`: 是否将参数转换为 JSON 请求体

#### 头部配置格式：
```json
{
  "headers": [
    {
      "key": "Content-Type",
      "value": "application/json"
    },
    {
      "key": "Authorization",
      "value": "Bearer {{.token}}"
    }
  ]
}
```

#### 查询参数配置格式：
```json
{
  "queryParams": [
    {
      "key": "userId",
      "value": "${userId}"
    },
    {
      "key": "page",
      "value": "${page}"
    }
  ]
}
```

## 使用说明

1. **参数类型**: 支持 `string`, `integer`, `number`, `boolean`, `array`, `object`
2. **必填参数**: 通过 `required` 字段控制
3. **超时设置**: 通过 `timeout` 字段设置（毫秒）
4. **请求头**: 使用 `key` 和 `value` 字段配置
5. **查询参数**: 使用 `queryParams` 数组配置，值使用 `${paramName}` 格式
6. **路径参数**: 在 URL 中使用 `{{.paramName}}` 格式
7. **参数映射**: 通过 `argsPosition` 指定参数位置：`path`、`query`、`body`

## 最佳实践

1. **从简单开始**: 先配置基本的 GET/POST 示例，逐步增加复杂度
2. **超时配置**: 
   - 简单查询: 30秒
   - 复杂操作: 45-120秒
   - 文件上传: 60秒以上
3. **错误处理**: 在业务层实现重试机制和合适的错误响应
4. **安全性**: 
   - 生产环境必须使用 HTTPS
   - 访问安全接口时包含认证头
   - 验证输入参数
5. **性能优化**: 
   - 设置合适的超时时间
   - 高频调用使用连接池
   - 监控和记录API性能

这些示例基于 **shenyu-examples-http** 项目中的真实接口，可以直接在 Shenyu 环境中使用和测试。

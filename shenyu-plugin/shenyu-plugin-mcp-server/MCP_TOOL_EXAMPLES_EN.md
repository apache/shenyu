# MCP Tool Configuration Examples

This document provides comprehensive examples of tool configurations for the Shenyu MCP Server Plugin based on **real interfaces from the shenyu-examples-http project**, covering different HTTP request methods, parameter types, and configuration patterns.

## 1. Simple GET Request Examples

### 1.1 GET Request with No Parameters
```json
{
  "name": "hello",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/hello\",\"method\":\"GET\",\"headers\":[],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "Shenyu client hello endpoint"
}
```

### 1.2 GET Request with Parameters
```json
{
  "name": "hi",
  "parameters": [
    {
      "name": "name",
      "type": "string",
      "description": "User name",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/hi\",\"method\":\"GET\",\"headers\":[],\"timeout\":30000,\"queryParams\":[{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"name\":\"query\"}}",
  "description": "Shenyu client hi endpoint with parameter"
}
```

## 2. User-Related GET Request Examples

### 2.1 Find User by ID
```json
{
  "name": "findByUserId",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "User ID",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByUserId\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"userId\",\"value\":\"${userId}\"}]},\"argsPosition\":{\"userId\":\"query\"}}",
  "description": "Find user information by user ID"
}
```

### 2.2 Find User by ID and Name
```json
{
  "name": "findByUserIdName",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "User ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "User name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByUserIdName\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"userId\",\"value\":\"${userId}\"},{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"userId\":\"query\",\"name\":\"query\"}}",
  "description": "Find user by ID and name"
}
```

### 2.3 Find Users with Pagination
```json
{
  "name": "findByPage",
  "parameters": [
    {
      "name": "keyword",
      "type": "string",
      "description": "Search keyword",
      "required": false
    },
    {
      "name": "page",
      "type": "integer",
      "description": "Page number",
      "required": false
    },
    {
      "name": "pageSize",
      "type": "integer",
      "description": "Page size",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/findByPage\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"keyword\",\"value\":\"${keyword}\"},{\"key\":\"page\",\"value\":\"${page}\"},{\"key\":\"pageSize\",\"value\":\"${pageSize}\"}]},\"argsPosition\":{\"keyword\":\"query\",\"page\":\"query\",\"pageSize\":\"query\"}}",
  "description": "Find users with pagination"
}
```

### 2.4 Get User by Path Parameter
```json
{
  "name": "getUserByPath",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "User ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "User name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/path/{{.id}}\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"name\",\"value\":\"${name}\"}]},\"argsPosition\":{\"id\":\"path\",\"name\":\"query\"}}",
  "description": "Get user information by path parameter"
}
```

## 3. POST Request Examples

### 3.1 User Payment Interface
```json
{
  "name": "payment",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "User ID",
      "required": true
    },
    {
      "name": "userName",
      "type": "string",
      "description": "User name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/payment\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"userId\":\"body\",\"userName\":\"body\"}}",
  "description": "User payment interface"
}
```

### 3.2 WAF Pass Test
```json
{
  "name": "wafPass",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/waf/pass\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "WAF pass test"
}
```

### 3.3 Shenyu Client POST Request
```json
{
  "name": "postHi",
  "parameters": [
    {
      "name": "name",
      "type": "string",
      "description": "User name",
      "required": false
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/shenyu/client/post/hi\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/x-www-form-urlencoded\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"name\":\"body\"}}",
  "description": "Shenyu client POST hi endpoint"
}
```

## 4. PUT Request Examples

### 4.1 Update User Information
```json
{
  "name": "updateUser",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "User ID",
      "required": true
    },
    {
      "name": "userId",
      "type": "string",
      "description": "User ID",
      "required": true
    },
    {
      "name": "userName",
      "type": "string",
      "description": "User name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/putPathBody/{{.id}}\",\"method\":\"PUT\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"id\":\"path\",\"userId\":\"body\",\"userName\":\"body\"}}",
  "description": "Update user information"
}
```

## 5. Order-Related Examples

### 5.1 Save Order
```json
{
  "name": "saveOrder",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "Order ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "Order name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/save\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"id\":\"body\",\"name\":\"body\"}}",
  "description": "Save order"
}
```

### 5.2 Find Order by ID
```json
{
  "name": "findOrderById",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "Order ID",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/findById\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000,\"queryParams\":[{\"key\":\"id\",\"value\":\"${id}\"}]},\"argsPosition\":{\"id\":\"query\"}}",
  "description": "Find order by ID"
}
```

### 5.3 RESTful Order Query
```json
{
  "name": "getOrderByPath",
  "parameters": [
    {
      "name": "id",
      "type": "string",
      "description": "Order ID",
      "required": true
    },
    {
      "name": "name",
      "type": "string",
      "description": "Order name",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/path/{{.id}}/{{.name}}\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{\"id\":\"path\",\"name\":\"path\"}}",
  "description": "RESTful style order query"
}
```

## 6. Request Processing Examples

### 6.1 Request Header Test
```json
{
  "name": "testRequestHeader",
  "parameters": [
    {
      "name": "headerKey1",
      "type": "string",
      "description": "Request header parameter",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/header\",\"method\":\"GET\",\"headers\":[{\"key\":\"header_key1\",\"value\":\"{{.headerKey1}}\"}],\"timeout\":30000},\"argsPosition\":{\"headerKey1\":\"query\"}}",
  "description": "Request header test"
}
```

### 6.2 Request Parameter Test
```json
{
  "name": "testRequestParameter",
  "parameters": [
    {
      "name": "parameterKey1",
      "type": "string",
      "description": "Request parameter",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/parameter\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/x-www-form-urlencoded\"}],\"timeout\":30000,\"argsToJsonBody\":true},\"argsPosition\":{\"parameterKey1\":\"body\"}}",
  "description": "Request parameter test"
}
```

### 6.3 Cookie Test
```json
{
  "name": "testRequestCookie",
  "parameters": [
    {
      "name": "userId",
      "type": "string",
      "description": "User ID (Cookie value)",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/request/cookie\",\"method\":\"GET\",\"headers\":[{\"key\":\"Cookie\",\"value\":\"userId={{.userId}}\"}],\"timeout\":30000},\"argsPosition\":{\"userId\":\"query\"}}",
  "description": "Cookie test"
}
```

## 7. File Upload Examples

### 7.1 Single File Upload
```json
{
  "name": "uploadSingleFile",
  "parameters": [
    {
      "name": "file",
      "type": "string",
      "description": "File content (Base64 encoded)",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/upload/webFluxSingle\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"multipart/form-data\"}],\"timeout\":60000,\"argsToJsonBody\":true},\"argsPosition\":{\"file\":\"body\"}}",
  "description": "Single file upload"
}
```

### 7.2 Multiple Files Upload
```json
{
  "name": "uploadMultipleFiles",
  "parameters": [
    {
      "name": "files",
      "type": "array",
      "description": "Multiple file contents (Base64 encoded array)",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/upload/webFluxFiles\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"multipart/form-data\"}],\"timeout\":60000,\"argsToJsonBody\":true},\"argsPosition\":{\"files\":\"body\"}}",
  "description": "Multiple files upload"
}
```

## 8. Advanced Feature Examples

### 8.1 OAuth2 Authentication Test
```json
{
  "name": "testOAuth2",
  "parameters": [
    {
      "name": "token",
      "type": "string",
      "description": "OAuth2 access token",
      "required": true
    }
  ],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/order/oauth2/test\",\"method\":\"GET\",\"headers\":[{\"key\":\"Authorization\",\"value\":\"Bearer {{.token}}\"}],\"timeout\":30000},\"argsPosition\":{\"token\":\"query\"}}",
  "description": "OAuth2 authentication test"
}
```

### 8.2 Cache Test
```json
{
  "name": "testCache",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/cache\",\"method\":\"GET\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":30000},\"argsPosition\":{}}",
  "description": "Cache test"
}
```

### 8.3 Big Object Response Test
```json
{
  "name": "testBigObject",
  "parameters": [],
  "requestConfig": "{\"requestTemplate\":{\"url\":\"/test/bigObject\",\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\",\"value\":\"application/json\"}],\"timeout\":60000},\"argsPosition\":{}}",
  "description": "Big object response test"
}
```

## Parameter Position Mapping (argsPosition)

### Supported Mapping Locations:

1. **URL Path Parameters**: `"path"` - Corresponds to `{{.paramName}}` in URL template
2. **Query Parameters**: `"query"` - Corresponds to parameters in `queryParams` array
3. **Request Body**: `"body"` - Used with `argsToJsonBody: true`

### Mapping Examples:
```json
{
  "argsPosition": {
    "id": "path",                        // Path parameter /test/path/{{.id}}
    "userId": "query",                   // Query parameter queryParams: [{"key": "userId", "value": "${userId}"}]
    "userName": "body"                   // JSON Body (requires argsToJsonBody: true)
  }
}
```

### Configuration Structure:

#### requestTemplate Configuration Items:
- `url`: URL template with path parameter support using `{{.paramName}}` placeholders
- `method`: HTTP method (GET, POST, PUT, DELETE)
- `headers`: Headers array, each element contains `key` and `value`
- `queryParams`: Query parameters array, each element contains `key` and `value`
- `timeout`: Timeout in milliseconds
- `argsToJsonBody`: Whether to convert parameters to JSON request body

#### Header Configuration Format:
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

#### Query Parameters Configuration Format:
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

## Usage Guidelines

1. **Parameter Types**: Supports `string`, `integer`, `number`, `boolean`, `array`, `object`
2. **Required Parameters**: Controlled by the `required` field
3. **Timeout Settings**: Configure timeout values in milliseconds via the `timeout` field
4. **Request Headers**: Configure using `key` and `value` fields
5. **Query Parameters**: Configure using `queryParams` array with `${paramName}` format values
6. **Path Parameters**: Use `{{.paramName}}` format in URL
7. **Parameter Mapping**: Use `argsPosition` to specify parameter location: `path`, `query`, `body`

## Best Practices

### 1. **Start Simple**
Begin with basic GET/POST examples and gradually add complexity.

### 2. **Timeout Configuration**
- Simple queries: 30 seconds
- Complex operations: 45-120 seconds
- File uploads: 60+ seconds

### 3. **Error Handling**
Implement retry mechanisms and proper error responses at the business layer.

### 4. **Security**
- Always use HTTPS in production
- Include authentication headers when accessing secure endpoints
- Validate input parameters

### 5. **Performance**
- Set appropriate timeouts
- Use connection pooling for high-frequency calls
- Monitor and log API performance

These examples are based on **real interfaces from the shenyu-examples-http project** and can be used and tested directly in a Shenyu environment.


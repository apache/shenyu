# AI Response Transformer Plugin

## 概述

AI Response Transformer Plugin 是 Apache ShenYu 网关的一个插件，用于使用大语言模型（LLM）对HTTP响应进行智能转换。该插件可以在响应返回给客户端之前，通过AI模型对响应内容进行修改、格式化或增强。

## 功能特性

- **智能响应转换**: 使用配置的AI模型对HTTP响应进行智能转换
- **灵活的配置**: 支持插件级别和规则级别的配置
- **多种AI提供商**: 支持OpenAI、Claude等多种AI模型提供商
- **响应头和响应体转换**: 可以同时转换响应头和响应体
- **缓存机制**: 复用AI模型客户端，提高性能

## 设计架构

### 复用的组件

该插件复用了以下组件，确保代码的一致性和可维护性：

1. **ChatClientCache**: 复用AI模型客户端缓存机制
2. **AiModelFactoryRegistry**: 复用AI模型工厂注册表
3. **AiCommonConfig**: 复用AI通用配置结构
4. **配置转换逻辑**: 复用配置转换方法

### 核心组件

1. **AiResponseTransformerPlugin**: 主插件类，负责响应转换逻辑
2. **AiResponseTransformerPluginHandler**: 插件处理器，负责配置管理
3. **AiResponseTransformerTemplate**: 模板类，负责构建AI请求消息
4. **AiResponseTransformerConfig**: 插件配置类
5. **AiResponseTransformerHandle**: 规则处理器类

## 工作流程

1. **请求处理**: 插件首先让请求通过插件链处理
2. **响应获取**: 获取上游服务的响应内容
3. **AI请求构建**: 将请求和响应信息构建成AI请求消息
4. **AI调用**: 调用配置的AI模型进行响应转换
5. **响应解析**: 解析AI返回的HTTP响应格式
6. **响应更新**: 更新原始响应的头部和体内容

## 配置说明

### 插件配置 (AiResponseTransformerConfig)

```json
{
  "provider": "openai",
  "baseUrl": "https://api.openai.com/v1",
  "apiKey": "your-api-key",
  "model": "gpt-3.5-turbo",
  "content": "请将响应内容转换为更友好的格式"
}
```

### 规则配置 (AiResponseTransformerHandle)

```json
{
  "provider": "openai",
  "baseUrl": "https://api.openai.com/v1",
  "apiKey": "your-api-key",
  "model": "gpt-3.5-turbo",
  "content": "请将响应内容转换为更友好的格式"
}
```

## 使用示例

### 1. 添加依赖

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-plugin-ai-response-transformer</artifactId>
    <version>${shenyu.version}</version>
</dependency>
```

### 2. 配置插件

在ShenYu Admin中配置插件：

- 插件名称: `aiResponseTransformer`
- 插件配置: 设置AI模型提供商、API密钥等信息

### 3. 配置规则

为特定API配置转换规则，指定转换提示词。

## AI响应格式

插件期望AI模型返回标准的HTTP/1.1响应格式：

```
HTTP/1.1 200 OK
Content-Type: application/json
Cache-Control: no-cache

{"status":"success","data":{"message":"Hello World"}}
```

## 与Request Transformer的关系

该插件与 `aiRequestTransformer` 插件形成互补：

- **Request Transformer**: 在请求发送到上游服务之前进行转换
- **Response Transformer**: 在响应返回给客户端之前进行转换

两个插件可以独立使用，也可以组合使用，实现完整的请求-响应转换流程。

## 注意事项

1. **性能考虑**: AI调用会增加响应时间，建议合理配置
2. **错误处理**: 插件包含完善的错误处理机制
3. **资源管理**: 使用缓存机制管理AI客户端连接
4. **配置验证**: 插件会验证必要的配置参数

## 扩展性

该插件设计具有良好的扩展性：

- 支持新的AI模型提供商
- 支持自定义响应转换逻辑
- 支持插件级别的配置覆盖 
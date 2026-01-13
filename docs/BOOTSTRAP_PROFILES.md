# ShenYu Bootstrap Build Profiles

This document describes the Maven profiles available for building `shenyu-bootstrap`.

## Overview

To optimize startup time and package size, `shenyu-bootstrap` dependencies have been modularized into Maven profiles. You can choose which set of plugins to include in your build.

## Profiles

| Profile ID | Description | Included Components | Activation |
|------------|-------------|---------------------|------------|
| `standard` | **Default**. Common Gateway features. | RateLimiter, Resilience4j, WAF, Sign, JWT, OAuth2, Metrics, WebSocket, all Data Sync methods. | **Active by default** |
| `minimal` | Core Gateway only. | Param-mapping, Request/Response transformation, Context-path, Cryptor. | Use `-P !standard` |
| `rpc` | RPC support. | Apache Dubbo, Sofa, Motan, Tars, gRPC, TCP. | Manual (`-P rpc`) |
| `logging` | Advanced logging. | RocketMQ, Kafka, RabbitMQ, Elasticsearch, Pulsar, ClickHouse, etc. | Manual (`-P logging`) |
| `ai` | AI Proxy support. | AI Proxy, AI Prompt, Token Limiter, Transformers. | Manual (`-P ai`) |
| `mcp` | Model Context Protocol. | MCP Server plugin. | Manual (`-P mcp`) |

## Usage Examples

### 1. Build Standard Version (Default)
Includes common plugins and all data sync options.
```bash
mvn clean package -pl shenyu-bootstrap -am
```

### 2. Build Minimal Version
Only the core gateway runtime. Useful for lightweight sidecars or simple proxies.
```bash
mvn clean package -pl shenyu-bootstrap -am -P !standard
```

### 3. Build with RPC Support
Standard features + Dubbo, Sofa, etc.
```bash
mvn clean package -pl shenyu-bootstrap -am -P rpc
```

### 4. Build Full Version (All Features)
```bash
mvn clean package -pl shenyu-bootstrap -am -P rpc,logging,ai,mcp
```
(Note: `standard` is included by default, so you don't need to list it unless you disabled it)

### 5. Custom Combination
Example: Minimal core + Dubbo + Kafka Logging
```bash
mvn clean package -pl shenyu-bootstrap -am -P !standard,rpc,logging
```

# How to run this example

1. start shenyu-admin

2. start shenyu-examples-http

3. add dependency to `/shenyu-bootstrap/pom.xml`

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-examples-plugin-wasm</artifactId>
    <version>${project.version}</version>
</dependency>
```

4. start shenyu-bootstrap

5. send GET request

```http request
GET http://localhost:9195/http/order/findById?id=123
Accept: application/json
Content-Type: application/json
```

---
title: filter扩展
keywords: soul
description: filter扩展
---


## 说明

* 本文是说明,如何进行 `org.springframework.web.server.WebFliter` 的扩展。


##  跨域支持

*  新增 `org.dromara.soul.bootstrap.cors.CrossFilter` 实现 WebFilter。

 ```java
 public class CrossFilter implements WebFilter {
 
     private static final String ALLOWED_HEADERS = "x-requested-with, authorization, Content-Type, Authorization, credential, X-XSRF-TOKEN,token,username,client";
 
     private static final String ALLOWED_METHODS = "*";
 
     private static final String ALLOWED_ORIGIN = "*";
 
     private static final String ALLOWED_EXPOSE = "*";
 
     private static final String MAX_AGE = "18000";
 
     @Override
     @SuppressWarnings("all")
     public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
         ServerHttpRequest request = exchange.getRequest();
         if (CorsUtils.isCorsRequest(request)) {
             ServerHttpResponse response = exchange.getResponse();
             HttpHeaders headers = response.getHeaders();
             headers.add("Access-Control-Allow-Origin", ALLOWED_ORIGIN);
             headers.add("Access-Control-Allow-Methods", ALLOWED_METHODS);
             headers.add("Access-Control-Max-Age", MAX_AGE);
             headers.add("Access-Control-Allow-Headers", ALLOWED_HEADERS);
             headers.add("Access-Control-Expose-Headers", ALLOWED_EXPOSE);
             headers.add("Access-Control-Allow-Credentials", "true");
             if (request.getMethod() == HttpMethod.OPTIONS) {
                 response.setStatusCode(HttpStatus.OK);
                 return Mono.empty();
             }
         }
         return chain.filter(exchange);
     }
 }
```
* 将 CrossFilter 注册成为 spring的bean,完事.


## 网关过滤 springboot健康检查

* 注意顺序，使用 `@Order` 注解

```java
@Component
@Order(-99)
public final class HealthFilter implements WebFilter {

    private static final String[] FILTER_TAG = {"/actuator/health", "/health_check"};

    @Override
    public Mono<Void> filter(@Nullable final ServerWebExchange exchange, @Nullable final WebFilterChain chain) {
        ServerHttpRequest request = Objects.requireNonNull(exchange).getRequest();
        String urlPath = request.getURI().getPath();
        for (String check : FILTER_TAG) {
            if (check.equals(urlPath)) {
                String result = JsonUtils.toJson(new Health.Builder().up().build());
                DataBuffer dataBuffer = exchange.getResponse().bufferFactory().wrap(result.getBytes());
                return exchange.getResponse().writeWith(Mono.just(dataBuffer));
            }
        }
        return Objects.requireNonNull(chain).filter(exchange);
    }
}

```

##  继承 `org.dromara.soul.web.filter.AbstractWebFilter`

* 新增一个类，继承它。

* 实现它的2个方法。

```java
   /**
     * this is Template Method ,children Implement your own filtering logic.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Boolean>} result：TRUE (is pass)，and flow next filter；FALSE (is not pass) execute doDenyResponse(ServerWebExchange exchange)
     */
    protected abstract Mono<Boolean> doFilter(ServerWebExchange exchange, WebFilterChain chain);

    /**
     * this is Template Method ,children Implement your own And response client.
     *
     * @param exchange the current server exchange.
     * @return {@code Mono<Void>} response msg.
     */
    protected abstract Mono<Void> doDenyResponse(ServerWebExchange exchange);
```
* `doFilter` 方法返回 Mono<true> 表示通过,反之则不通过，不通过的时候，会调用 `doDenyResponse`,输出相关信息到前端。





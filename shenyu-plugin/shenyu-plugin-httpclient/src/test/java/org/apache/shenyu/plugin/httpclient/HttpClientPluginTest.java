package org.apache.shenyu.plugin.httpclient;

import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * @author Jerry
 * @Date 2025/3/16 22:46
 */
public class HttpClientPluginTest extends AbstractRetryableHttpClientPlugin {

    @Override
    protected Mono<Void> doHttpRequest(ServerWebExchange exchange) {
         WebClient.create()
                .method(exchange.getRequest().getMethod())
                .uri(exchange.getRequest().getURI())
                .headers(headers -> headers.addAll(exchange.getRequest().getHeaders()))
                .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                .exchangeToMono(clientResponse -> {
                    exchange.getResponse().setStatusCode(clientResponse.statusCode());
                    return clientResponse.bodyToMono(byte[].class)
                            .doOnNext(bytes -> exchange.getResponse().writeWith(Flux.just(
                                    exchange.getResponse().bufferFactory().wrap(bytes))));
                });
            return null;
    }

    @Override
    public int getOrder() {
        return 0;
    }

    @Override
    public boolean skip(ServerWebExchange exchange) {
        return false;
    }
}

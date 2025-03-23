package org.apache.shenyu.plugin.httpclient;

import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.mockito.Mockito.mock;

/**
 * @author Jerry
 * @Date 2025/3/16 22:46
 */
public class RetryStrategyTest{

    @Test
    void testDefaultRetryBackoffExecute() {
        // 创建一个模拟的 AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        ExponentialRetryBackoffStrategy<String> strategy = new ExponentialRetryBackoffStrategy<>(httpClientPlugin);

        // 创建一个模拟的 ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // 创建一个模拟的响应 Mono，它会抛出异常
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // 执行重试策略
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // 使用 StepVerifier 验证结果
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testDefaultRetryStrategyExecute() {
        // 创建一个模拟的 AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(httpClientPlugin);

        // 创建一个模拟的 ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // 创建一个模拟的响应 Mono，它会抛出异常
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // 执行重试策略
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // 使用 StepVerifier 验证结果
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testFixedRetryStrategyExecute() {
        // 创建一个模拟的 AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);

        // 创建一个模拟的 ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // 创建一个模拟的响应 Mono，它会抛出异常
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // 执行重试策略
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // 使用 StepVerifier 验证结果
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }
}

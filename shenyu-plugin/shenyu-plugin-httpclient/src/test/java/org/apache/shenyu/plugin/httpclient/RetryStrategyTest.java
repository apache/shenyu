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
        // Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        ExponentialRetryBackoffStrategy<String> strategy = new ExponentialRetryBackoffStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testDefaultRetryStrategyExecute() {
        //Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testFixedRetryStrategyExecute() {
        // Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }
}

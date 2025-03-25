package org.apache.shenyu.plugin.httpclient;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 *  Custom retry policy.
 *      Also please achieve your own
 *@author Jerry
 *@Date 2025/3/23 14:27
 */
public class CustomRetryStrategy<R> implements RetryStrategy<R> {
    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    private static final Logger LOG = LoggerFactory.getLogger(CustomRetryStrategy.class);

    public CustomRetryStrategy(AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * Custom retry policy.
     *
     * @param response   he Mono object of the response
     * @param exchange   Current Server Exchange Object
     * @param duration   Timeout
     * @param retryTimes Number of retries
     * @return Response Mono object after retry processing
     */
    public Mono<R> execute(Mono<R> response, ServerWebExchange exchange, Duration duration, int retryTimes) {
        return null;
    }
}

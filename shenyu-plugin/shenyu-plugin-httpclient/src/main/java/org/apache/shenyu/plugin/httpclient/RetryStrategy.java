package org.apache.shenyu.plugin.httpclient;


import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 * Retry Policy Interface.
 * @param <R> Request Response Type
 *@author Jerry
 *@Date 2025/3/23 08:27
 */
public interface RetryStrategy<R> {
    /**
     * Execute retry policy
     *
     * @param clientResponse Original Request Response
     * @param exchange       Server Exchange Object
     * @param duration       Timeout
     * @param retryTimes     Number of retries
     * @return Number of retries
     */
    Mono<R> execute(Mono<R> clientResponse, ServerWebExchange exchange, Duration duration, int retryTimes);
}

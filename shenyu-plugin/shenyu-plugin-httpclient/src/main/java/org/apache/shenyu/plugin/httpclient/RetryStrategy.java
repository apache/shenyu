package org.apache.shenyu.plugin.httpclient;


import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 * 重试策略接口 Retry Policy Interface
 * @param <R> 请求响应类型 Request Response Type
 *@author Jerry
 *@Date 2025/3/23 08:27
 */
public interface RetryStrategy<R> {
    /**
     * 执行重试策略 Execute retry policy
     *
     * @param clientResponse 原始请求响应 Original Request Response
     * @param exchange       服务器交换对象 Server Exchange Object
     * @param duration       超时时间 Timeout
     * @param retryTimes     重试次数 Number of retries
     * @return 重试后的响应 Number of retries
     */
    Mono<R> execute(Mono<R> clientResponse, ServerWebExchange exchange, Duration duration, int retryTimes);
}

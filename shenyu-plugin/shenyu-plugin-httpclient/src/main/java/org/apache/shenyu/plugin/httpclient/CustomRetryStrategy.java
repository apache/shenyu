package org.apache.shenyu.plugin.httpclient;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 * 自定义重试策略 Custom retry policy
 *      还请自己实现
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
     * 自定义重试策略 Custom retry policy
     *
     * @param response   响应的 Mono 对象 The Mono object of the response
     * @param exchange   当前服务器交换对象 Current Server Exchange Object
     * @param duration   超时时间 Timeout
     * @param retryTimes 重试次数 Number of retries
     * @return 经过重试处理后的响应 Mono 对象 Response Mono object after retry processing
     */
    public Mono<R> execute(Mono<R> response, ServerWebExchange exchange, Duration duration, int retryTimes) {
        return null;
    }
}

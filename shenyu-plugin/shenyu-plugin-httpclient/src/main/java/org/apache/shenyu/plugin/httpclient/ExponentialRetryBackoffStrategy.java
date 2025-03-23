package org.apache.shenyu.plugin.httpclient;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.retry.Retry;
import reactor.util.retry.RetryBackoffSpec;

import java.time.Duration;

/**
 * @author Jerry
 * @Date 2025/3/23 14:20
 */
public class ExponentialRetryBackoffStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(ExponentialRetryBackoffStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public ExponentialRetryBackoffStrategy(AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * 执行重试策略
     *
     * @param response    响应的 Mono 对象
     * @param exchange    当前服务器交换对象
     * @param duration    超时时间
     * @param retryTimes  重试次数
     * @return 经过重试处理后的响应 Mono 对象
     */
    public Mono<R> execute(Mono<R> response, ServerWebExchange exchange, Duration duration, int retryTimes) {
        RetryBackoffSpec retrySpec = initDefaultBackoff(retryTimes);
        return response.retryWhen(retrySpec)
                .timeout(duration, Mono.error(() -> new java.util.concurrent.TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
    }

    private RetryBackoffSpec initDefaultBackoff(int retryTimes) {
        return Retry.backoff(retryTimes, Duration.ofMillis(500))
                .maxBackoff(Duration.ofSeconds(5))
                // 只对瞬时错误进行重试
                .transientErrors(true)
                // 添加 50% 的随机抖动到每次重试的延迟时间
                .jitter(0.5d)
                .filter(t -> t instanceof IllegalStateException)
                // 当达到最大重试次数后抛出一个指定的异常
                .onRetryExhaustedThrow((retryBackoffSpecErr, retrySignal) -> {
                    throw new IllegalStateException("重试超限");
                });
    }
}

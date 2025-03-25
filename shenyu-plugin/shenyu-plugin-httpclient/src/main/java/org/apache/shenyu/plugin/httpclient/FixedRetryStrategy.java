package org.apache.shenyu.plugin.httpclient;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.retry.Retry;

import java.time.Duration;

/**
 * @author Jerry
 * @Date 2025/3/23 10:04
 */
public class FixedRetryStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(FixedRetryStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public FixedRetryStrategy(AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * Execute retry policy
     *
     * @param response   The Mono object of the response
     * @param exchange   Current Server Exchange Object
     * @param duration   TIMEOUT
     * @param retryTimes Number of retries
     * @return Response Mono object after retry processing
     */
    public Mono<R> execute(Mono<R> response, ServerWebExchange exchange, Duration duration, int retryTimes) {
        Retry retrySpec = initFixedBackoff(retryTimes);
        return response.retryWhen(retrySpec)
                .timeout(duration, Mono.error(() -> new java.util.concurrent.TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
    }

    private Retry initFixedBackoff(int retryTimes) {
        return Retry.fixedDelay(retryTimes, Duration.ofSeconds(2));
    }
}

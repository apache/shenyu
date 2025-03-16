package org.apache.shenyu.plugin.httpclient;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.reactivestreams.Publisher;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.retry.Retry;

import java.time.Duration;
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Predicate;

/**
 * 抽象HTTP客户端插件（带重试能力）
 */
public abstract class AbstractRetryableHttpClientPlugin implements ShenyuPlugin {

    // 默认配置参数
    protected int maxRetries = 3;
    protected Duration initialBackoff = Duration.ofMillis(500);
    protected RetryStrategy retryStrategy = RetryStrategy.DEFAULT;
    protected Predicate<Throwable> retryPredicate = e -> e instanceof IllegalStateException;
    protected Duration[] customBackoffSequence;

    /**
     * 核心执行方法
     */
    @Override
    public Mono<Void> execute(ServerWebExchange exchange, ShenyuPluginChain chain) {
        return doExecuteWithRetry(exchange)
                .then(chain.execute(exchange))  // 继续执行插件链
                .onErrorResume(this::handleFinalError);
    }

    /**
     * 带重试的执行流程
     */
    protected Mono<Void> doExecuteWithRetry(ServerWebExchange exchange) {
        return doHttpRequest(exchange)
                .retryWhen(buildRetrySpec());
    }

    /**
     * 构建重试策略
     */
    protected Retry buildRetrySpec() {
        switch (retryStrategy) {
            case FIXED:
                return Retry.fixedDelay(maxRetries, initialBackoff)
                        .filter(retryPredicate);
            case CUSTOM:
                // 自定义序列重试策略
                return null;
            default:
                return Retry.backoff(maxRetries, initialBackoff)
                        .maxBackoff(Duration.ofSeconds(5))
                        .jitter(0.5)
                        .filter(retryPredicate);
        }
    }

    /**
     * HTTP请求抽象方法
     */
    protected abstract Mono<Void> doHttpRequest(ServerWebExchange exchange);

    /**
     * 最终错误处理
     */
    protected Mono<Void> handleFinalError(Throwable throwable) {
        // 可在此记录监控指标或设置exchange属性
        return Mono.error(throwable);
    }

    /**
     * 自定义序列重试策略
     */
    private static class CustomSequenceRetry extends Retry {
        private final int maxAttempts;
        private final Duration[] backoffSequence;

        CustomSequenceRetry(int maxAttempts, Duration[] backoffSequence) {
            this.maxAttempts = maxAttempts;
            this.backoffSequence = Optional.ofNullable(backoffSequence)
                    .orElse(new Duration[]{Duration.ofSeconds(1)});
        }

        @Override
        public Publisher<Long> generateCompanion(Flux<RetrySignal> flux) {
            return flux.flatMap(this::getDelay);
        }

        private Publisher<Long> getDelay(RetrySignal rs) {
            long iteration = rs.totalRetries() + 1;

            if (iteration > maxAttempts) {
                return Flux.error(rs.failure());
            }

            int index = (int) ((iteration-1) % backoffSequence.length);
            Duration delay = backoffSequence[index];

            return Flux.just(rs.totalRetries())
                    .delayElements(delay)
                    .doOnNext(t -> logRetry(rs, delay));
        }

        private void logRetry(RetrySignal rs, Duration delay) {
            System.out.printf("Retry %d/%d after %dms%n",
                    rs.totalRetries()+1, maxAttempts, delay.toMillis());
        }
    }

    // 保持原有配置方法
    public void setMaxRetries(int maxRetries) {
        this.maxRetries = maxRetries;
    }

    public void setInitialBackoff(Duration initialBackoff) {
        this.initialBackoff = initialBackoff;
    }

    public void setRetryStrategy(RetryStrategy retryStrategy) {
        this.retryStrategy = retryStrategy;
    }

    public void setCustomBackoffSequence(Duration[] customBackoffSequence) {
        this.customBackoffSequence = Arrays.copyOf(customBackoffSequence, customBackoffSequence.length);
    }

    public void setRetryPredicate(Predicate<Throwable> retryPredicate) {
        this.retryPredicate = retryPredicate;
    }

    public enum RetryStrategy {
        DEFAULT,
        FIXED,
        CUSTOM
    }
}
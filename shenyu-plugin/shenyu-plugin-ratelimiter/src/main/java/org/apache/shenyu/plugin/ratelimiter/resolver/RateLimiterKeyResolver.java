package org.apache.shenyu.plugin.ratelimiter.resolver;

import org.apache.shenyu.spi.SPI;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * RateLimiterKeyResolver.
 */
@SPI
public interface RateLimiterKeyResolver {

    /**
     * get KeyResolver's name.
     * @return KeyResolver's name.
     */
    String getKeyResolverName();

    /**
     * resolve.
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @return {@code Mono<String>} to indicate when resolve is complete
     */
    Mono<String> resolve(ServerWebExchange exchange);
}

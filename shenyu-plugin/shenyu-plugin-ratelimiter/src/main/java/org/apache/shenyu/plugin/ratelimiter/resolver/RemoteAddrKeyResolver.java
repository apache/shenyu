package org.apache.shenyu.plugin.ratelimiter.resolver;

import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

@Join
public class RemoteAddrKeyResolver implements RateLimiterKeyResolver {

    @Override
    public String getKeyResolverName() {
        return null;
    }

    @Override
    public Mono<String> resolve(ServerWebExchange exchange) {
        return null;
    }
}

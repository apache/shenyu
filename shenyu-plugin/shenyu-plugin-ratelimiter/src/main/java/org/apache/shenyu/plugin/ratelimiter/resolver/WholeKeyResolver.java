package org.apache.shenyu.plugin.ratelimiter.resolver;

import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

@Join
public class WholeKeyResolver implements RateLimiterKeyResolver {

    @Override
    public String getKeyResolverName() {
        return "whole_key_resolver";
    }

    @Override
    public String resolve(ServerWebExchange exchange) {
        return Mono.just("whole_key_resolver");
    }
}

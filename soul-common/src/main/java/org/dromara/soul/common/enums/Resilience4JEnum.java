package org.dromara.soul.common.enums;


import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum Resilience4JEnum {

    /**
     * circuitBreaker waf enum.
     */
    circuitBreaker(0, "reject"),

    /**
     * ratelimiter  enum.
     */
    ratelimiter(1, "allow"),

    /**
     * circuitBreaker &  ratelimiter.
     */
    both(2, "both"),
    ;

    private final int code;

    private final String name;
}

package org.apache.shenyu.common.enums;

import java.util.Arrays;

public enum HttpRetryBackoffSpecEnum {
    /**
     * 默认重试
     */
    DEFAULT_BACKOFF("default"),

    /**
     * 固定重试
     */
    FIXED_BACKOFF("fixed"),

    /**
     * 指数重试
     */
    EXPONENTIAL_BACKOFF("exponential"),

    /**
     * 自定义重试
     */
    CUSTOM_BACKOFF("custom");

    private final String name;


    HttpRetryBackoffSpecEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static HttpRetryBackoffSpecEnum acquireByName(final String name) {
        return Arrays.stream(HttpRetryBackoffSpecEnum.values())
                .filter(e -> e.getName().equals(name)).findFirst()
                .orElse(HttpRetryBackoffSpecEnum.DEFAULT_BACKOFF);
    }

    public static String getDefault() {
        return DEFAULT_BACKOFF.getName();
    }

}

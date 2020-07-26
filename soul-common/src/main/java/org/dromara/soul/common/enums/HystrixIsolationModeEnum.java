package org.dromara.soul.common.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * hystrix execution isolation strategy.
 *
 * @author liangziqiang
 */
@RequiredArgsConstructor
@Getter
public enum HystrixIsolationModeEnum {
    /**
     * thread pool mode.
     */
    @SuppressWarnings("checkstyle:WhitespaceAfter") THREAD_POOL(0, "thread"),
    /**
     * semaphore mode.
     */
    @SuppressWarnings("checkstyle:WhitespaceAfter") SEMAPHORE(1, "semaphore");

    private final int code;

    private final String name;
}

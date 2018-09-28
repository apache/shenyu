package org.dromara.soul.common.dto.convert;

import lombok.Getter;
import lombok.Setter;

/**
 * @author xiaoyu(549477611 @ qq.com)
 */
@Getter
@Setter
public class HystrixHandle {

    /**
     * hystrix group key is required.
     */
    private String groupKey;

    /**
     * hystrix command key is required.
     */
    private String commandKey;

    /**
     * hystrix withExecutionIsolationSemaphoreMaxConcurrentRequests.
     */
    private int maxConcurrentRequests;

    /**
     * hystrix  withCircuitBreakerErrorThresholdPercentage.
     */
    private int errorThresholdPercentage;

    /**
     * hystrix withCircuitBreakerRequestVolumeThreshold.
     */
    private int requestVolumeThreshold;

    /**
     * hystrix withCircuitBreakerSleepWindowInMilliseconds.
     */
    private int sleepWindowInMilliseconds;

    /**
     *  timeout is required.
     */
    private Integer timeout;
}

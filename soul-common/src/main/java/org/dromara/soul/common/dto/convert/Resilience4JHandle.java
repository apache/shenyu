package org.dromara.soul.common.dto.convert;

import lombok.Data;

/**
 * this is Circuit plugin handle.
 *
 * @author zhanglei
 */
@Data
public class Resilience4JHandle {

    private String id;
    private String fallbackUri;

    /**
     * time out config.
     */
    private long timeoutDuration;
    private int slidingWindowSize;
    private int slidingWindowType;
    private int minimumNumberOfCalls;
    private int slowCallDurationThreshold;
    private int maxWaitDurationInHalfOpenState;
    private int waitIntervalFunctionInOpenState;
    private int permittedNumberOfCallsInHalfOpenState;

    private float failureRateThreshold;
    private float slowCallRateThreshold;

    private boolean writableStackTraceEnabled = true;
    private boolean automaticTransitionFromOpenToHalfOpenEnabled = false;

    //限流器相关
    private int timeoutDurationRate;
    private int limitRefreshPeriod;
    private int limitForPeriod;




}

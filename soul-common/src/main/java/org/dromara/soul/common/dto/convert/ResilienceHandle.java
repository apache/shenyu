package org.dromara.soul.common.dto.convert;

import lombok.Data;
import org.dromara.soul.common.constant.Constants;

/**
 * this is Circuit plugin handle.
 *
 * @author zhanglei
 */
@Data
public class ResilienceHandle {


    /**
     * ratelimiter timeoutDurationRate.
     * 获取token超时时间
     */
    private int timeoutDurationRate = Constants.TIMEOUT_DURATION_RATE;

    /**
     * ratelimiter limitRefreshPeriod.
     * 生成token的周期
     */
    private int limitRefreshPeriod = Constants.LIMIT_REFRESH_PERIOD;

    /**
     * ratelimiter limitForPeriod.
     * 每次生成token多少个
     */
    private int limitForPeriod = Constants.LIMIT_FOR_PERIOD;

    /**
     * circuitBreaker circuitEnable.
     * 是否开启熔断
     */
    private int circuitEnable = Constants.CIRCUIT_ENABLE;

    /**
     * circuitBreaker timeoutDuration.
     * 超时时间
     */
    private long timeoutDuration = Constants.TIMEOUT_DURATION;

    /**
     * circuitBreaker timeoutDuration.
     * 降级url
     */
    private String fallbackUri;

    /**
     * circuitBreaker slidingWindowSize.
     * 滑动窗口大小
     */
    private int slidingWindowSize = Constants.SLIDING_WINDOW_SIZE;

    /**
     * circuitBreaker slidingWindowType
     * 滑动窗口类型 O:TIME_BASED 基于时间 1:COUNT_BASED基于计数
     */
    private int slidingWindowType = Constants.SLIDING_WINDOW_TYPE;

    /**
     * circuitBreaker minimumNumberOfCalls.
     * 最小统计请求阈值
     */
    private int minimumNumberOfCalls = Constants.MINIMUM_NUMBER_OF_CALLS;

    /**
     * circuitBreaker waitIntervalFunctionInOpenState.
     * 熔断器打开持续时间默认60s
     */
    private int waitIntervalFunctionInOpenState = Constants.WAIT_INTERVAL_FUNCTION_IN_OPEN_STATE;

    /**
     * circuitBreaker waitIntervalFunctionInOpenState.
     * 熔断器halfopen的调用数
     */
    private int permittedNumberOfCallsInHalfOpenState = Constants.PERMITTED_NUMBER_OF_CALLS_IN_HALF_OPEN_STATE;

    /**
     * circuitBreaker failureRateThreshold.
     * 错误率(百分比)
     */
    private float failureRateThreshold = Constants.FAILURE_RATE_THRESHOLD;

    /**
     * circuitBreaker automaticTransitionFromOpenToHalfOpenEnabled.
     * 是否自动打开半开状态
     */
    private Boolean automaticTransitionFromOpenToHalfOpenEnabled = Constants.AUTOMATIC_TRANSITION_FROM_OPEN_TO_HALF_OPEN_ENABLED;


}

package org.dromara.soul.web.plugin.hystrix;

import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixObservableCommand;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.HystrixHandle;

/**
 * the hystrix builder.
 *
 * @author xiaoyu(549477611 @ qq.com)
 */
public class HystrixBuilder {

    /**
     * this is build HystrixObservableCommand.Setter.
     *
     * @param hystrixHandle {@linkplain HystrixHandle}
     * @return {@linkplain HystrixObservableCommand.Setter}
     */
    public static HystrixObservableCommand.Setter build(final HystrixHandle hystrixHandle) {

        if (hystrixHandle.getMaxConcurrentRequests() == 0) {
            hystrixHandle.setMaxConcurrentRequests(Constants.MAX_CONCURRENT_REQUESTS);
        }
        if (hystrixHandle.getErrorThresholdPercentage() == 0) {
            hystrixHandle.setErrorThresholdPercentage(Constants.ERROR_THRESHOLD_PERCENTAGE);
        }
        if (hystrixHandle.getRequestVolumeThreshold() == 0) {
            hystrixHandle.setRequestVolumeThreshold(Constants.REQUEST_VOLUME_THRESHOLD);
        }
        if (hystrixHandle.getSleepWindowInMilliseconds() == 0) {
            hystrixHandle.setSleepWindowInMilliseconds(Constants.SLEEP_WINDOW_INMILLISECONDS);
        }

        HystrixCommandGroupKey groupKey = HystrixCommandGroupKey.Factory.asKey(hystrixHandle.getGroupKey());

        HystrixCommandKey commandKey = HystrixCommandKey.Factory.asKey(hystrixHandle.getCommandKey());

        final HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        .withExecutionTimeoutInMilliseconds(hystrixHandle.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.SEMAPHORE)
                        .withExecutionIsolationSemaphoreMaxConcurrentRequests(hystrixHandle.getMaxConcurrentRequests())
                        .withCircuitBreakerErrorThresholdPercentage(hystrixHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(hystrixHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(hystrixHandle.getSleepWindowInMilliseconds());

        return HystrixObservableCommand.Setter
                .withGroupKey(groupKey)
                .andCommandKey(commandKey)
                .andCommandPropertiesDefaults(propertiesSetter);
    }

}

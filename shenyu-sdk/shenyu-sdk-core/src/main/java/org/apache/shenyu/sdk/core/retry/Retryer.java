/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.sdk.core.retry;

import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;

import java.util.Objects;

import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * Cloned for each invocation to {@link ShenyuSdkClient#execute(ShenyuRequest)} .
 * Implementations may keep state to determine if retry operations should continue or not.
 * Reference to feign.Retryer.
 */
public interface Retryer extends Cloneable {

    /**
     * Implementation that never retries request. It propagates the RetryableException.
     */
    Retryer NEVER_RETRY = new Retryer() {

        @Override
        public void continueOrPropagate(final RetryableException e) {
            throw e;
        }

        @Override
        public Retryer instance() {
            return this;
        }

        @Override
        public int retryCount() {
            return 0;
        }

    };

    /**
     * if retry is permitted, return (possibly after sleeping), otherwise propagate the exception.
     *
     * @param e RetryableException
     */
    void continueOrPropagate(RetryableException e);

    /**
     * clone current retryer.
     *
     * @return {@linkplain Retryer}
     */
    Retryer instance();

    /**
     * retryCount.
     *
     * @return int
     */
    int retryCount();

    class DefaultRetry implements Retryer {

        private final int maxAttempts;

        private final long period;

        private final long maxPeriod;

        private int attempt;

        private long sleptForMillis;

        public DefaultRetry() {
            this(100, SECONDS.toMillis(1), 5);
        }

        public DefaultRetry(final long period, final long maxPeriod, final int maxAttempts) {
            this.period = period;
            this.maxPeriod = maxPeriod;
            this.maxAttempts = maxAttempts;
            this.attempt = 1;
        }

        public void continueOrPropagate(final RetryableException e) {
            if (attempt++ >= maxAttempts) {
                throw e;
            }

            long interval;
            if (Objects.nonNull(e.retryAfter())) {
                interval = e.retryAfter().getTime() - System.currentTimeMillis();
                if (interval > maxPeriod) {
                    interval = maxPeriod;
                }
                if (interval < 0) {
                    return;
                }
            } else {
                interval = nextMaxInterval();
            }
            try {
                Thread.sleep(interval);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
                throw e;
            }
            sleptForMillis += interval;
        }

        /**
         * Calculates the time interval to a retry attempt. <br>
         * The interval increases exponentially with each attempt, at a rate of nextInterval *= 1.5
         * (where 1.5 is the backoff factor), to the maximum interval.
         *
         * @return time in milliseconds from now until the next attempt.
         */
        long nextMaxInterval() {
            long interval = (long) (period * Math.pow(1.5, attempt - 1));
            return Math.min(interval, maxPeriod);
        }

        @Override
        public Retryer instance() {
            return new DefaultRetry(period, maxPeriod, maxAttempts);
        }

        /**
         * retry count.
         *
         * @return {@link int}
         */
        @Override
        public int retryCount() {
            return attempt;
        }
    }

}

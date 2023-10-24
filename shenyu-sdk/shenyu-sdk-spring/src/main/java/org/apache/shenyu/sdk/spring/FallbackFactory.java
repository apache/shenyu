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

package org.apache.shenyu.sdk.spring;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Used to control the fallback given its cause.
 * Ex.
 *
 * <pre>
 * {@code
 * // This instance will be invoked if there are errors of any kind.
 * FallbackFactory<GitHub> fallbackFactory = cause -> (owner, repo) -> {
 *   if (cause instanceof ShenyuException && ((ShenyuException) cause).status() == 403) {
 *     return Collections.emptyList();
 *   } else {
 *     return Arrays.asList("yogi");
 *   }
 * };
 *
 * GitHub github = ShenyuCircuitBreaker.builder()
 *                             ...
 *                             .target(GitHub.class, "https://api.github.com", fallbackFactory);
 * }
 * </pre>
 *
 * @param <T> the Shenyu interface type
 */
public interface FallbackFactory<T> {

    /**
     * Returns an instance of the fallback appropriate for the given cause.
     * @param cause cause of an exception.
     * @return fallback
     */
    T create(Throwable cause);

    final class Default<T> implements FallbackFactory<T> {

        private final Log logger;

        private final T constant;

        public Default(final T constant) {
            this(constant, LogFactory.getLog(Default.class));
        }

        Default(final T constant, final Log logger) {
            this.constant = checkNotNull(constant, "fallback");
            this.logger = checkNotNull(logger, "logger");
        }

        @Override
        public T create(final Throwable cause) {
            if (logger.isTraceEnabled()) {
                logger.trace("fallback due to: " + cause.getMessage(), cause);
            }
            return constant;
        }

        @Override
        public String toString() {
            return constant.toString();
        }

    }

}

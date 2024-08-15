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

package org.apache.shenyu.plugin.httpclient.exception;


/**
 * Shenyu request timeout exception.
 */
public final class ShenyuTimeoutException extends RuntimeException {

    private static final long serialVersionUID = -6123954847415409614L;

    /**
     * Instantiates a new Shenyu request timeout exception.
     *
     * @param e the e
     */
    public ShenyuTimeoutException(final Throwable e) {
        super(e);
    }

    /**
     * Instantiates a new Shenyu request timeout exception.
     *
     * @param message the message
     */
    public ShenyuTimeoutException(final String message) {
        super(message);
    }

    /**
     * Instantiates a new Shenyu request timeout exception.
     *
     * @param message   the message
     * @param throwable the throwable
     */
    public ShenyuTimeoutException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}

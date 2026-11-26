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

package org.apache.shenyu.plugin.base.circuitbreaker;

import org.apache.commons.lang3.exception.ExceptionUtils;

/**
 * A rejected connection acquisition before an upstream request was sent.
 */
public final class LocalPoolRejectionException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    /**
     * Mark a local pool rejection while retaining its original cause.
     *
     * @param cause the rejected acquisition
     */
    public LocalPoolRejectionException(final Throwable cause) {
        super(cause.getMessage(), cause);
    }

    /**
     * Check for a marked rejection, including one wrapped by another plugin.
     *
     * @param error the received error
     * @return whether the error resulted from a local pool rejection
     */
    public static boolean isLocalRejection(final Throwable error) {
        return ExceptionUtils.indexOfType(error, LocalPoolRejectionException.class) >= 0;
    }
}

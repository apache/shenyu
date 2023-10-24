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

package org.apache.shenyu.plugin.sign.api;

import java.util.Objects;

@FunctionalInterface
public interface VerifySupplier {

    /**
     * Verifies and gets a result.
     * @return result
     */
    VerifyResult verify();

    /**
     * Returns a composed verifySupplier that represents a short-circuiting logical AND of this verifySupplier and another. When evaluating the composed verifySupplier,
     * if this verifyResult is failed, then the other verifySupplier is not evaluated.
     * @param other a verifySupplier that will be logically-ANDed with this verifySupplier
     * @return a composed verifySupplier
     */
    default VerifySupplier and(VerifySupplier other) {
        Objects.requireNonNull(other);
        return () -> {
            VerifyResult verifyResult = verify();
            if (verifyResult.isFailed()) {
                return verifyResult;
            }
            return other.verify();
        };
    }

    /**
     * Returns itself, this mainly helps to write smooth code.
     * @param verifySupplier verifySupplier
     * @return verifySupplier
     */
    static VerifySupplier apply(VerifySupplier verifySupplier) {
        return verifySupplier;
    }
}

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

package org.apache.shenyu.e2e.engine.scenario.specification;

import org.apache.shenyu.e2e.engine.scenario.function.Checker;
import org.apache.shenyu.e2e.engine.scenario.function.Deleter;
import org.apache.shenyu.e2e.engine.scenario.function.HttpChecker;
import org.apache.shenyu.e2e.engine.scenario.function.HttpWaiting;
import org.apache.shenyu.e2e.engine.scenario.function.WaitForHelper;
import org.apache.shenyu.e2e.engine.scenario.function.Waiting;
import org.jetbrains.annotations.NotNull;

/**
 * ShenYu after each specification.
 */
public class ShenYuAfterEachSpec implements AfterEachSpec {
    
    public static final ShenYuAfterEachSpec DEFAULT = new ShenYuAfterEachSpec(Deleter.DEFAULT, Checker.DEFAULT, Waiting.DEFAULT);
    
    private final Deleter deleter;
    
    private final Checker postChecker;

    private final Waiting deleteWaiting;
    
    public ShenYuAfterEachSpec(final Deleter deleter, final Checker postChecker, final Waiting deleteWaiting) {
        this.deleter = deleter;
        this.postChecker = postChecker;
        this.deleteWaiting = deleteWaiting;
    }

    /**
     * builder.
     *
     * @return {@link ShenYuAfterEachSpecBuilder}
     */
    public static ShenYuAfterEachSpec.ShenYuAfterEachSpecBuilder builder() {
        return new ShenYuAfterEachSpec.ShenYuAfterEachSpecBuilder();
    }

    @Override
    public Deleter getDeleter() {
        return deleter;
    }

    @Override
    public Checker getPostChecker() {
        return postChecker;
    }

    @Override
    public Waiting deleteWaiting() {
        return deleteWaiting;
    }

    public static class ShenYuAfterEachSpecBuilder {

        private Deleter deleter = Deleter.DEFAULT;

        private Checker checker = Checker.DEFAULT;

        private Waiting deleteWaiting = Waiting.DEFAULT;

        /**
         * builder set checker.
         * @param checker checker
         * @return ShenYuAfterEachSpecBuilder
         */
        public ShenYuAfterEachSpecBuilder checker(final @NotNull Checker checker) {
            this.checker = checker;
            return this;
        }

        /**
         * builder set waiting with checker.
         * @param checker checker
         * @return ShenYuAfterEachSpecBuilder
         */
        public ShenYuAfterEachSpecBuilder deleteWaiting(final @NotNull Checker checker) {
            this.deleteWaiting = (HttpWaiting) supplier -> WaitForHelper.waitForEffecting(supplier, (HttpChecker) checker);
            return this;
        }

        /**
         * builder set deleter.
         * @param deleter deleter
         * @return ShenYuAfterEachSpecBuilder
         */
        public ShenYuAfterEachSpecBuilder deleter(final @NotNull Deleter deleter) {
            this.deleter = deleter;
            return this;
        }

        /**
         * builder build before each spec.
         * @return ShenYuAfterEachSpec
         */
        public ShenYuAfterEachSpec build() {
            return new ShenYuAfterEachSpec(deleter, checker, deleteWaiting);
        }
    }
}

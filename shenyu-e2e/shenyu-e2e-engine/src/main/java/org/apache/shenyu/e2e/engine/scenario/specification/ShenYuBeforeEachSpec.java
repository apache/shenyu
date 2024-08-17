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

import org.apache.shenyu.e2e.engine.scenario.function.HttpChecker;
import org.apache.shenyu.e2e.engine.scenario.function.HttpWaiting;
import org.apache.shenyu.e2e.engine.scenario.function.WaitForHelper;
import io.restassured.http.Method;
import io.restassured.specification.ResponseSpecification;
import org.apache.shenyu.e2e.model.ResourcesData;
import org.apache.shenyu.e2e.model.ResourcesData.ResourcesDataBuilder;
import org.apache.shenyu.e2e.model.data.BindingData;
import org.apache.shenyu.e2e.model.data.RuleData;
import org.apache.shenyu.e2e.model.data.SelectorData;
import org.apache.shenyu.e2e.engine.scenario.function.Checker;
import org.apache.shenyu.e2e.engine.scenario.function.Waiting;
import org.jetbrains.annotations.NotNull;

/**
 * ShenYu before each specification.
 */
public class ShenYuBeforeEachSpec implements BeforeEachSpec {

    private final Checker checker;

    private final ResourcesData resources;

    private final Waiting waiting;

    ShenYuBeforeEachSpec(final Checker checker, final ResourcesData resources, final Waiting waiting) {
        this.checker = checker;
        this.resources = resources;
        this.waiting = waiting;
    }

    /**
     * builder.
     * @return ShenYuBeforeEachSpecBuilder
     */
    public static ShenYuBeforeEachSpecBuilder builder() {
        return new ShenYuBeforeEachSpecBuilder();
    }

    /**
     * get checker.
     *
     * @return checker
     */
    @Override
    public Checker getChecker() {
        return checker;
    }

    /**
     * get resources.
     *
     * @return  resources
     */
    @Override
    public ResourcesData getResources() {
        return resources;
    }

    /**
     * get waiting.
     *
     * @return waiting
     */
    @Override
    public Waiting getWaiting() {
        return waiting;
    }

    public static class ShenYuBeforeEachSpecBuilder {

        private final ResourcesDataBuilder builder = ResourcesData.builder();

        private Checker checker = Checker.DEFAULT;

        private Waiting waiting = Waiting.DEFAULT;

        /**
         * builder set checker.
         * @param checker checker
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder checker(final @NotNull Checker checker) {
            this.checker = checker;
            return this;
        }

        /**
         * builder add selector and rule.
         * @param selector selector
         * @param rules rules
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder addSelectorAndRule(final SelectorData selector, final RuleData... rules) {
            builder.add(selector, rules);
            return this;
        }

        /**
         * addSelectorAndRule.
         *
         * @param selector selector
         * @param bindingData bindingData
         * @param rules rules
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder addSelectorAndRule(final SelectorData selector, final BindingData bindingData, final RuleData... rules) {
            builder.add(selector, bindingData, rules);
            return this;
        }
        
        /**
         * builder set waiting.
         * @param waiting waiting
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder waiting(final @NotNull Waiting waiting) {
            this.waiting = waiting;
            return this;
        }
        
        /**
         * builder set waiting.
         * @param method method
         * @param endpoint endpoint
         * @param expected expected
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder waiting(final Method method, final String endpoint, final ResponseSpecification expected) {
            this.waiting = (HttpWaiting) supplier -> WaitForHelper.waitForEffecting(supplier, method, endpoint, expected);
            return this;
        }
        
        /**
         * builder set waiting with checker.
         * @param checker checker
         * @return ShenYuBeforeEachSpecBuilder
         */
        public ShenYuBeforeEachSpecBuilder waiting(final @NotNull Checker checker) {
            this.waiting = (HttpWaiting) supplier -> WaitForHelper.waitForEffecting(supplier, (HttpChecker) checker);
            return this;
        }
        
        /**
         * builder build before each spec.
         * @return ShenYuBeforeEachSpec
         */
        public ShenYuBeforeEachSpec build() {
            return new ShenYuBeforeEachSpec(checker, builder.build(), waiting);
        }
    }
}

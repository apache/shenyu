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

package org.apache.shenyu.e2e.testcase.common.specification;

import io.restassured.http.Method;
import io.restassured.specification.ResponseSpecification;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.shenyu.e2e.client.admin.model.ResourcesData;
import org.apache.shenyu.e2e.client.admin.model.ResourcesData.ResourcesDataBuilder;
import org.apache.shenyu.e2e.client.admin.model.data.RuleData;
import org.apache.shenyu.e2e.client.admin.model.data.SelectorData;
import org.apache.shenyu.e2e.engine.scenario.function.Checker;
import org.apache.shenyu.e2e.engine.scenario.function.Waiting;
import org.apache.shenyu.e2e.engine.scenario.specification.BeforeEachSpec;
import org.apache.shenyu.e2e.testcase.common.function.HttpChecker;
import org.apache.shenyu.e2e.testcase.common.function.HttpWaiting;
import org.apache.shenyu.e2e.testcase.common.function.WaitForHelper;
import org.jetbrains.annotations.NotNull;

@Getter
@AllArgsConstructor(access = AccessLevel.PACKAGE)
public class ShenYuBeforeEachSpec implements BeforeEachSpec {
    private final Checker checker;
    private final ResourcesData resources;
    private final Waiting waiting;
    
    public static ShenYuBeforeEachSpecBuilder builder() {
        return new ShenYuBeforeEachSpecBuilder();
    }
    
    public static class ShenYuBeforeEachSpecBuilder {
        private final ResourcesDataBuilder builder = ResourcesData.builder();
        private Checker checker = Checker.DEFAULT;
        private Waiting waiting = Waiting.DEFAULT;
        
        public ShenYuBeforeEachSpecBuilder checker(@NotNull Checker checker) {
            this.checker = checker;
            return this;
        }
        
        public ShenYuBeforeEachSpecBuilder addSelectorAndRule(SelectorData selector, RuleData... rules) {
            builder.add(selector, rules);
            return this;
        }
        
        public ShenYuBeforeEachSpecBuilder waiting(@NotNull Waiting waiting) {
            this.waiting = waiting;
            return this;
        }
        
        public ShenYuBeforeEachSpecBuilder waiting(Method method, String endpoint, ResponseSpecification expected) {
            this.waiting = (HttpWaiting) supplier -> WaitForHelper.waitForEffecting(supplier, method, endpoint, expected);
            return this;
        }
        
        public ShenYuBeforeEachSpecBuilder waiting(@NotNull Checker checker) {
            this.waiting = (HttpWaiting) supplier -> {
                WaitForHelper.waitForEffecting(supplier, (HttpChecker) checker);
            };
            return this;
        }
        
        public ShenYuBeforeEachSpec build() {
            return new ShenYuBeforeEachSpec(checker, builder.build(), waiting);
        }
    }
}

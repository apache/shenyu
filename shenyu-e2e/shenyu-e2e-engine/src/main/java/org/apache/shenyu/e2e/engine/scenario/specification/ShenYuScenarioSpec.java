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

import com.google.common.base.Strings;
import org.apache.shenyu.e2e.common.IdGenerator;
import org.junit.jupiter.api.Assertions;

/**
 * ShenYu scenario specification.
 */
public class ShenYuScenarioSpec implements ScenarioSpec {
    
    private final String name;
    
    private final BeforeEachSpec beforeEachSpec;
    
    private final CaseSpec caseSpec;
    
    private final AfterEachSpec afterEachSpec;

    public ShenYuScenarioSpec(final String name, final BeforeEachSpec beforeEachSpec, final CaseSpec caseSpec, final AfterEachSpec afterEachSpec) {
        this.name = name;
        this.beforeEachSpec = beforeEachSpec;
        this.caseSpec = caseSpec;
        this.afterEachSpec = afterEachSpec;
    }

    /**
     * get name.
     *
     * @return name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * get beforeEachSpec.
     *
     * @return beforeEachSpec
     */
    @Override
    public BeforeEachSpec getBeforeEachSpec() {
        return beforeEachSpec;
    }

    /**
     * get caseSpec.
     *
     * @return caseSpec
     */
    @Override
    public CaseSpec getCaseSpec() {
        return caseSpec;
    }

    /**
     * get afterEachSpec.
     *
     * @return afterEachSpec
     */
    @Override
    public AfterEachSpec getAfterEachSpec() {
        return afterEachSpec;
    }
    
    /**
     * scenario specification builder.
     * @return ShenYuScenarioSpecBuilder
     */
    public static ShenYuScenarioSpecBuilder builder() {
        return new ShenYuScenarioSpecBuilder();
    }
    
    public static class ShenYuScenarioSpecBuilder {

        private String name;

        private BeforeEachSpec beforeEachSpec = BeforeEachSpec.DEFAULT;

        private AfterEachSpec afterEachSpec = AfterEachSpec.DEFAULT;
        
        private CaseSpec caseSpec;
        
        /**
         * name.
         * @param name name
         * @return ShenYuScenarioSpecBuilder
         */
        public ShenYuScenarioSpecBuilder name(final String name) {
            this.name = name;
            return this;
        }
        
        /**
         * before each spec.
         * @param beforeEachSpec beforeEachSpec
         * @return ShenYuScenarioSpecBuilder
         */
        public ShenYuScenarioSpecBuilder beforeEachSpec(final BeforeEachSpec beforeEachSpec) {
            this.beforeEachSpec = beforeEachSpec;
            return this;
        }
        
        /**
         * case spec.
         * @param caseSpec caseSpec
         * @return ShenYuScenarioSpecBuilder
         */
        public ShenYuScenarioSpecBuilder caseSpec(final CaseSpec caseSpec) {
            this.caseSpec = caseSpec;
            return this;
        }
        
        /**
         * after each spec.
         * @param afterEachSpec afterEachSpec
         * @return ShenYuScenarioSpecBuilder
         */
        public ShenYuScenarioSpecBuilder afterEachSpec(final AfterEachSpec afterEachSpec) {
            this.afterEachSpec = afterEachSpec;
            return this;
        }
        
        /**
         * build.
         * @return ShenYuScenarioSpec
         */
        public ShenYuScenarioSpec build() {
            Assertions.assertNotNull(caseSpec, "CaseSpec is required.");
            if (Strings.isNullOrEmpty(name)) {
                name = "shenyu-" + IdGenerator.generateTestId();
            }
            return new ShenYuScenarioSpec(name, beforeEachSpec, caseSpec, afterEachSpec);
        }
    }
    
}

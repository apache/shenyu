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

package org.apache.shenyu.e2e.engine.scenario.template;

import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuAfterEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;

/**
 * build shenyu spec template.
 */
public class ShenYuSpecTemplate {
    
    /**
     * build shenyu spec.
     *
     * @param name name
     * @param beforeEachSpec beforeEachSpec
     * @param caseSpec caseSpec
     * @param afterEachSpec afterEachSpec
     * @return ShenYuScenarioSpec
     */
    public static ShenYuScenarioSpec buildShenYuSpec(final String name, final ShenYuBeforeEachSpec beforeEachSpec, final ShenYuCaseSpec caseSpec, final ShenYuAfterEachSpec afterEachSpec) {
        return ShenYuScenarioSpec.builder()
                .name(name)
                .beforeEachSpec(beforeEachSpec)
                .caseSpec(caseSpec)
                .afterEachSpec(afterEachSpec)
                .build();
    }
}

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

package org.apache.shenyu.e2e.testcase.motan;

import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;

import java.util.List;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;

public class MotanPluginCases implements ShenYuScenarioProvider {

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testMotanHi()
        );
    }

    /**
     * test with uri equal.
     *
     * @return ShenYuScenarioSpec
     */
    private ShenYuScenarioSpec testMotanHi() {
        return ShenYuScenarioSpec.builder()
                .name("motan test")
                .beforeEachSpec(ShenYuBeforeEachSpec.builder()
                        .checker(exists("/motan/demo/hi"))
                        .build())
                .caseSpec(ShenYuCaseSpec.builder()
                        .addExists("/motan/demo/hi")
                        .addNotExists("/motan/demo/h")
                        .build())
                .build();
    }
}

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

import org.apache.shenyu.e2e.client.admin.model.ResourcesData;
import org.apache.shenyu.e2e.engine.annotation.ShenYuScenarioParameter;
import org.apache.shenyu.e2e.engine.scenario.function.Checker;
import org.apache.shenyu.e2e.engine.scenario.function.Waiting;

@ShenYuScenarioParameter
public interface BeforeEachSpec {
    
    Checker getChecker();
    
    ResourcesData getResources();
    
    Waiting getWaiting();
    
    BeforeEachSpec DEFAULT = new BeforeEachSpec() {
        @Override
        public Checker getChecker() {
            return Checker.DEFAULT;
        }
    
        @Override
        public ResourcesData getResources() {
            return ResourcesData.builder().build();
        }
    
        @Override
        public Waiting getWaiting() {
            return Waiting.DEFAULT;
        }
    };
}

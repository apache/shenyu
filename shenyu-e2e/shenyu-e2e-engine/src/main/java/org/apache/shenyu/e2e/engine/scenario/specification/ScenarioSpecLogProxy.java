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

import lombok.AllArgsConstructor;
import org.apache.shenyu.e2e.client.admin.model.ResourcesData;
import org.apache.shenyu.e2e.engine.scenario.function.Checker;
import org.apache.shenyu.e2e.engine.scenario.function.Deleter;
import org.apache.shenyu.e2e.engine.scenario.function.Verifier;
import org.apache.shenyu.e2e.engine.scenario.function.Waiting;
import org.slf4j.MDC;

import java.util.List;

@AllArgsConstructor
public class ScenarioSpecLogProxy implements ScenarioSpec {
    private final ScenarioSpec spec;
    
    @Override
    public BeforeEachSpec getBeforeEachSpec() {
        return new BeforeEachSpec() {
            final BeforeEachSpec spec = ScenarioSpecLogProxy.this.spec.getBeforeEachSpec();
            
            @Override
            public Checker getChecker() {
                MDC.put("operate", "beforeCheck");
                return spec.getChecker();
            }
            
            @Override
            public ResourcesData getResources() {
                MDC.put("operate", "createSelectors");
                return spec.getResources();
            }
            
            @Override
            public Waiting getWaiting() {
                MDC.put("operate", "waitingFor");
                return spec.getWaiting();
            }
        };
    }
    
    @Override
    public CaseSpec getCaseSpec() {
        return new CaseSpec() {
            final CaseSpec spec = ScenarioSpecLogProxy.this.spec.getCaseSpec();
            
            @Override
            public String getName() {
                return spec.getName();
            }
            
            @Override
            public List<Verifier> getVerifiers() {
                MDC.put("operate", "verify");
                return spec.getVerifiers();
            }
        };
    }
    
    @Override
    public AfterEachSpec getAfterEachSpec() {
        return new AfterEachSpec() {
            final AfterEachSpec spec = ScenarioSpecLogProxy.this.spec.getAfterEachSpec();
            
            @Override
            public Deleter getDeleter() {
                MDC.put("operate", "deleteResource");
                return spec.getDeleter();
            }
            
            @Override
            public Checker getPostChecker() {
                MDC.put("operate", "postCheck");
                return spec.getPostChecker();
            }
        };
    }
    
    @Override
    public String getName() {
        return spec.getName();
    }
    
}

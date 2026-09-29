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

package org.apache.shenyu.admin.scale.scaler;

import org.apache.shenyu.admin.jpa.repository.ScalePolicyRepository;
import org.apache.shenyu.admin.jpa.repository.ScaleRuleRepository;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ScaleTaskInitializer implements CommandLineRunner {

    private final ScalePolicyRepository scalePolicyRepository;

    private final ScalePolicyCache scalePolicyCache;

    private final ScaleRuleRepository scaleRuleRepository;

    private final ScaleRuleCache scaleRuleCache;

    private final ScaleService scaleService;

    public ScaleTaskInitializer(final ScalePolicyRepository scalePolicyRepository,
                                final ScalePolicyCache scalePolicyCache,
                                final ScaleService scaleService,
                                final ScaleRuleRepository scaleRuleRepository,
                                final ScaleRuleCache scaleRuleCache) {
        this.scalePolicyRepository = scalePolicyRepository;
        this.scalePolicyCache = scalePolicyCache;
        this.scaleService = scaleService;
        this.scaleRuleRepository = scaleRuleRepository;
        this.scaleRuleCache = scaleRuleCache;
    }

    @Override
    public void run(final String... args) {
        List<ScalePolicyDO> policies = scalePolicyRepository.findAll();
        scalePolicyCache.initialize(policies);

        List<ScaleRuleDO> rules = scaleRuleRepository.findAll();
        scaleRuleCache.initialize(rules);

        scaleService.executeScaling();
    }
}

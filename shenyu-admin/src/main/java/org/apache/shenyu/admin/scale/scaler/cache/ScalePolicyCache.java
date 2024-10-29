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

package org.apache.shenyu.admin.scale.scaler.cache;

import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class ScalePolicyCache {

    private final ConcurrentHashMap<String, ScalePolicyDO> cache = new ConcurrentHashMap<>();

    /**
     * initialize.
     *
     * @param policies policies
     */
    public void initialize(final List<ScalePolicyDO> policies) {
        cache.clear();
        policies.forEach(policy -> cache.put(policy.getId(), policy));
    }

    /**
     * update policy.
     *
     * @param policy policy
     */
    public void updatePolicy(final ScalePolicyDO policy) {
        cache.put(policy.getId(), policy);
    }

    /**
     * get all policies.
     *
     * @return List
     */
    public List<ScalePolicyDO> getAllPolicies() {
        return cache.values().stream().toList();
    }

    /**
     * get policy by id.
     *
     * @param id id
     * @return ScalePolicyDO
     */
    public ScalePolicyDO getPolicyById(final String id) {
        return cache.get(id);
    }
}

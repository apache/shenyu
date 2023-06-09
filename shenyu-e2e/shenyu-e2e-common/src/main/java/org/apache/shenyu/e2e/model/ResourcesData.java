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

package org.apache.shenyu.e2e.model;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import org.apache.shenyu.e2e.model.data.RuleData;
import org.apache.shenyu.e2e.model.data.SelectorData;

import java.util.List;

/**
 * ResourcesData.
 */
public class ResourcesData {
    
    private final List<Resource> resources;

    private ResourcesData(List<Resource> resources) {
        this.resources = resources;
    }

    /**
     * get resources.
     *
     * @return
     */
    public List<Resource> getResources() {
        return resources;
    }

    @Override
    public String toString() {
        return "ResourcesData{" +
                "resources=" + resources +
                '}';
    }

    public static ResourcesDataBuilder builder() {
        return new ResourcesDataBuilder();
    }
    
    public static class ResourcesDataBuilder {
        private final Builder<Resource> resources = ImmutableList.builder();
        
        public ResourcesDataBuilder add(SelectorData selector, RuleData... rules) {
            resources.add(new Resource(selector, ImmutableList.<RuleData>builder().add(rules).build()));
            return this;
        }
        
        public ResourcesData build() {
            return new ResourcesData(resources.build());
        }
    }
    

    public static class Resource {
        
        private final SelectorData selector;
        
        private final List<RuleData> rules;

        private Resource(SelectorData selector, List<RuleData> rules) {
            this.selector = selector;
            this.rules = rules;
        }

        /**
         * get selector.
         *
         * @return selector
         */
        public SelectorData getSelector() {
            return selector;
        }

        /**
         * get rules.
         *
         * @return rules
         */
        public List<RuleData> getRules() {
            return rules;
        }

        @Override
        public String toString() {
            return "Resource{"
                    + "selector="
                    + selector
                    + ", rules="
                    + rules
                    + '}';
        }
    }
}

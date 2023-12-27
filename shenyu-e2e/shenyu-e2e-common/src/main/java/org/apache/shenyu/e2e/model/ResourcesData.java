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
import org.apache.shenyu.e2e.model.data.BindingData;
import org.apache.shenyu.e2e.model.data.RuleData;
import org.apache.shenyu.e2e.model.data.SelectorData;

import java.util.List;

/**
 * ResourcesData.
 */
public final class ResourcesData {

    private final List<Resource> resources;

    private ResourcesData(final List<Resource> resources) {
        this.resources = resources;
    }

    /**
     * get resources.
     *
     * @return List
     */
    public List<Resource> getResources() {
        return resources;
    }

    @Override
    public String toString() {
        return "ResourcesData{"
                + "resources=" + resources
                + '}';
    }

    /**
     * builder.
     *
     * @return ResourcesDataBuilder
     */
    public static ResourcesDataBuilder builder() {
        return new ResourcesDataBuilder();
    }

    public static class ResourcesDataBuilder {
        private final Builder<Resource> resources = ImmutableList.builder();

        /**
         * add resource data.
         *
         * @param selector selector
         * @param rules    rules
         * @return ResourcesDataBuilder
         */
        public ResourcesDataBuilder add(final SelectorData selector, final RuleData... rules) {
            resources.add(new Resource(selector, ImmutableList.<RuleData>builder().add(rules).build()));
            return this;
        }

        /**
         * add resource data.
         *
         * @param selector    selector
         * @param bindingData bindingData
         * @param rules       rules
         * @return ResourcesDataBuilder
         */
        public ResourcesDataBuilder add(final SelectorData selector, final BindingData bindingData, final RuleData... rules) {
            resources.add(new Resource(selector, bindingData, ImmutableList.<RuleData>builder().add(rules).build()));
            return this;
        }

        /**
         * build.
         *
         * @return ResourcesData
         */
        public ResourcesData build() {
            return new ResourcesData(resources.build());
        }
    }

    public static final class Resource {

        private final SelectorData selector;

        private final List<RuleData> rules;

        private BindingData bindingData;

        private Resource(final SelectorData selector, final List<RuleData> rules) {
            this.selector = selector;
            this.rules = rules;
        }

        private Resource(final SelectorData selector, final BindingData bindingData, final List<RuleData> rules) {
            this.selector = selector;
            this.rules = rules;
            this.bindingData = bindingData;
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

        /**
         * getBindingData.
         *
         * @return BindingData
         */
        public BindingData getBindingData() {
            return bindingData;
        }

        /**
         * setBindingData.
         *
         * @param bindingData bindingData
         */
        public void setBindingData(final BindingData bindingData) {
            this.bindingData = bindingData;
        }

        @Override
        public String toString() {
            return "Resource{"
                    + "selector="
                    + selector
                    + ", rules="
                    + rules
                    + ", bindingData="
                    + bindingData
                    + '}';
        }
    }
}

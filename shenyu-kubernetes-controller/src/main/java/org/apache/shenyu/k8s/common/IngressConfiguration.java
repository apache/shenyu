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

package org.apache.shenyu.k8s.common;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

public class IngressConfiguration {

    private final SelectorData selectorData;

    private final RuleData ruleData;

    private final MetaData metaData;

    /**
     * constructor.
     * @param selectorData selectorData
     * @param ruleData ruleData
     * @param metaData metaData
     */
    public IngressConfiguration(final SelectorData selectorData, final RuleData ruleData, final MetaData metaData) {
        this.selectorData = selectorData;
        this.ruleData = ruleData;
        this.metaData = metaData;
    }

    /**
     * get selectorData.
     * @return selectorData
     */
    public SelectorData getSelectorData() {
        return selectorData;
    }

    /**
     * get ruleData.
     * @return ruleData
     */
    public RuleData getRuleData() {
        return ruleData;
    }

    /**
     * get metaData.
     * @return metaData
     */
    public MetaData getMetaData() {
        return metaData;
    }
}

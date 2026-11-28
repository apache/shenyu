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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.validation.MatchingConditionEnforcement;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Startup properties for matching-condition write-boundary validation.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.matching-condition")
public class MatchingConditionProperties implements InitializingBean, DisposableBean {

    private boolean strictValidation;

    private boolean previousStrictValidation;

    /**
     * Returns whether known-invalid conditions are rejected.
     *
     * @return true when strict validation is enabled
     */
    public boolean isStrictValidation() {
        return strictValidation;
    }

    /**
     * Sets strict validation.
     *
     * @param strictValidation true to reject known-invalid conditions
     */
    public void setStrictValidation(final boolean strictValidation) {
        this.strictValidation = strictValidation;
    }

    @Override
    public void afterPropertiesSet() {
        previousStrictValidation = MatchingConditionEnforcement.isStrictValidation();
        MatchingConditionEnforcement.setStrictValidation(strictValidation);
    }

    @Override
    public void destroy() {
        MatchingConditionEnforcement.setStrictValidation(previousStrictValidation);
    }
}

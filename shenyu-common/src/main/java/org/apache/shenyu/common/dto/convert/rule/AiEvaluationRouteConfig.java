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

package org.apache.shenyu.common.dto.convert.rule;

import java.io.Serializable;

/**
 * Per-selector opt-in switch for AI evaluation sample export (issue #8256).
 *
 * <p>Both layers must opt in: the gateway-wide {@code shenyu.plugins.ai.evaluation.enabled}
 * property and this per-route switch. A route that has not opted in produces no
 * samples and holds no state. {@code includeIdentity} independently authorizes the
 * trusted identity fields of the sample; by default samples carry metadata only.</p>
 */
public class AiEvaluationRouteConfig implements Serializable {

    private static final long serialVersionUID = 6853991478251304634L;

    private Boolean enabled = false;

    private String sampleRate = "1";

    private Boolean includeIdentity = false;

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    public String getSampleRate() {
        return sampleRate;
    }

    public void setSampleRate(final String sampleRate) {
        this.sampleRate = sampleRate;
    }

    public Boolean getIncludeIdentity() {
        return includeIdentity;
    }

    public void setIncludeIdentity(final Boolean includeIdentity) {
        this.includeIdentity = includeIdentity;
    }
}

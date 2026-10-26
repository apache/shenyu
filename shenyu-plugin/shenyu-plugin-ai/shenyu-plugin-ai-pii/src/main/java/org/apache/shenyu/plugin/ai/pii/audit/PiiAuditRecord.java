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

package org.apache.shenyu.plugin.ai.pii.audit;

import java.util.Objects;

/**
 * One audit observation of a direction of a payload: what was detected and
 * what was done about it. The record never carries the detected value, the
 * scanned text or credentials, so any sink can log or export it without
 * leaking the content it reports on.
 */
public final class PiiAuditRecord {

    private final String piiType;

    private final int count;

    private final String direction;

    private final String action;

    private final String ruleId;

    /**
     * An audit observation.
     *
     * @param piiType the detector type observed
     * @param count how many detections of the type were observed
     * @param direction request or response
     * @param action the action the rule applied
     * @param ruleId the rule the observation came from
     */
    public PiiAuditRecord(final String piiType, final int count, final String direction,
                          final String action, final String ruleId) {
        this.piiType = piiType;
        this.count = count;
        this.direction = direction;
        this.action = action;
        this.ruleId = ruleId;
    }

    /**
     * get the detector type observed.
     *
     * @return the type
     */
    public String getPiiType() {
        return piiType;
    }

    /**
     * get how many detections were observed.
     *
     * @return the count
     */
    public int getCount() {
        return count;
    }

    /**
     * get the direction of the payload.
     *
     * @return request or response
     */
    public String getDirection() {
        return direction;
    }

    /**
     * get the applied action.
     *
     * @return the action name
     */
    public String getAction() {
        return action;
    }

    /**
     * get the rule the observation came from.
     *
     * @return the rule id
     */
    public String getRuleId() {
        return ruleId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PiiAuditRecord)) {
            return false;
        }
        PiiAuditRecord other = (PiiAuditRecord) o;
        return count == other.count
                && Objects.equals(piiType, other.piiType)
                && Objects.equals(direction, other.direction)
                && Objects.equals(action, other.action)
                && Objects.equals(ruleId, other.ruleId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(piiType, count, direction, action, ruleId);
    }

    @Override
    public String toString() {
        return "piiType=" + piiType + " count=" + count + " direction=" + direction
                + " action=" + action + " ruleId=" + ruleId;
    }
}

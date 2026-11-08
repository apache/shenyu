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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

/**
 * The outcome of one feedback join attempt (issue #8256). Every rejection names
 * its reason so the operator runner can distinguish a forged reference from an
 * expired one and from an unauthorized tenant.
 *
 * @param status join outcome
 * @param sampleId the joined sample reference
 * @param judgeId the judge provenance of the attempt
 */
public record AiFeedbackJoinResult(Status status, String sampleId, String judgeId) {

    /**
     * Join outcome vocabulary.
     */
    public enum Status {
        ACCEPTED, UPDATED, UNKNOWN_SAMPLE, TENANT_MISMATCH, REJECTED_INVALID, REJECTED_BOUND
    }
}

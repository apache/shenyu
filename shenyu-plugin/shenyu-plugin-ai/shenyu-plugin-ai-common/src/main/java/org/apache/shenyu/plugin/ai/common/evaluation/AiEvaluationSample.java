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

package org.apache.shenyu.plugin.ai.common.evaluation;

/**
 * One exported evaluation sample: a body-free metadata snapshot of one terminal AI
 * request (issue #8256). The {@code sampleId} is an opaque reference the gateway
 * minted; feedback joins authorize against it and the gateway, never against a
 * request id. Identity fields are {@code null} unless the route explicitly
 * allow-listed them; unknown facts are {@code null}, not zero or invented.
 *
 * @param sampleId opaque sample reference minted by the gateway
 * @param schemaVersion manifest contract version
 * @param exportedAtMillis export receipt time
 * @param requestId the logical request identifier of the observation
 * @param selectorId selector that served the request
 * @param ruleId rule that served the request, may be {@code null}
 * @param configurationVersion configuration generation the route acted on
 * @param outcome terminal classification name
 * @param httpStatus assigned upstream status, {@code null} before headers
 * @param elapsedMillis terminal duration
 * @param attemptCount upstream attempt count including repairs
 * @param promptTokens provider-reported prompt tokens, {@code null} when unknown
 * @param completionTokens provider-reported completion tokens, {@code null} when unknown
 * @param totalTokens provider-reported total tokens, {@code null} when unknown
 * @param usageStatus usage quality status name, {@code null} when absent
 * @param identityIncluded whether the trusted identity fields are present
 * @param tenant trusted tenant, {@code null} unless identity was allow-listed
 * @param subject trusted subject, {@code null} unless identity was allow-listed
 * @param target resolved target, {@code null} unless identity was allow-listed
 */
public record AiEvaluationSample(String sampleId, String schemaVersion, long exportedAtMillis,
                                 String requestId, String selectorId, String ruleId, Long configurationVersion,
                                 String outcome, Integer httpStatus, long elapsedMillis, int attemptCount,
                                 Long promptTokens, Long completionTokens, Long totalTokens, String usageStatus,
                                 boolean identityIncluded, String tenant, String subject, String target) {
}

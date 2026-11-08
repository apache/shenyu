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

import java.util.Map;

/**
 * One feedback join offered against an exported evaluation sample (issue #8256).
 * The join authorizes through the opaque {@code sampleId} plus the tenant the
 * gateway recorded for that sample; a request id is never an authority. Joins with
 * the same {@code judgeId} are idempotent updates, distinct judges append up to
 * the bounded per-sample join budget.
 *
 * @param sampleId the opaque sample reference being joined
 * @param tenant tenant the join claims; must match the recorded sample tenant
 * @param judgeId provenance of the judge or human submission, part of the join key
 * @param evalSetVersion eval-set version, {@code null} when unknown
 * @param judgeVersion judge version, {@code null} when unknown
 * @param labels bounded typed label set, never fabricated
 * @param submittedAtMillis join receipt time
 */
public record AiEvaluationFeedback(String sampleId, String tenant, String judgeId,
                                   String evalSetVersion, String judgeVersion,
                                   Map<String, String> labels, long submittedAtMillis) {
}

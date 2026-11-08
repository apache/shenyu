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
 * Versioned wire contract of the AI evaluation sample export (issue #8256).
 *
 * <p>Samples are metadata-only by construction: no prompt, completion or other
 * request content is captured. Trusted identity fields enter a sample only when
 * the route explicitly allow-lists them. Absent facts stay absent — a missing
 * version or usage figure is exported as unknown, never fabricated.</p>
 */
public final class AiEvaluationContract {

    /**
     * Version of the exported sample manifest.
     */
    public static final String SAMPLE_VERSION = "eval-sample-v1";

    /**
     * Version of the exported feedback join event.
     */
    public static final String FEEDBACK_VERSION = "eval-feedback-v1";

    /**
     * Maximum number of typed labels one feedback join may carry.
     */
    public static final int MAX_LABELS = 8;

    /**
     * Maximum characters of one label value.
     */
    public static final int MAX_LABEL_VALUE_CHARS = 256;

    private AiEvaluationContract() {
    }
}

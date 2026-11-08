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

import java.util.List;

/**
 * The export boundary of the AI evaluation pipeline (issue #8256). Implementations
 * receive batches of versioned manifest payloads from the exporter's dedicated
 * thread — never from the request path — and must not throw; a failing sink must
 * not decide the fate of the sampled request. The default implementation writes
 * one structured line per record so an operator-owned offline runner can consume
 * the stream from the gateway log infrastructure.
 */
public interface AiEvaluationSink extends AutoCloseable {

    /**
     * Export one batch of versioned records.
     *
     * @param records non-empty batch of versioned export records
     */
    void export(List<AiEvaluationExportRecord> records);

    @Override
    default void close() {
    }
}

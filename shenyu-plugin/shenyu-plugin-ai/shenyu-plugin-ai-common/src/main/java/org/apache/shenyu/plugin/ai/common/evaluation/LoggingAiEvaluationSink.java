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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Default {@link AiEvaluationSink}: one structured line per versioned record, the
 * consumption channel for an operator-owned offline evaluation runner.
 */
public class LoggingAiEvaluationSink implements AiEvaluationSink {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingAiEvaluationSink.class);

    @Override
    public void export(final List<AiEvaluationExportRecord> records) {
        for (AiEvaluationExportRecord record : records) {
            LOG.info("AI_EVALUATION|{}|{}|{}", record.type(), record.schemaVersion(), record.payloadJson());
        }
    }
}

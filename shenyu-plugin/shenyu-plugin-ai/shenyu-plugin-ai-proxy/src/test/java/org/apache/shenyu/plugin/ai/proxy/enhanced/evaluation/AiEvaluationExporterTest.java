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

import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationExportRecord;
import org.apache.shenyu.plugin.ai.common.evaluation.AiEvaluationSink;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests of the bounded asynchronous exporter, driven through
 * {@code drainOnce} so no test waits on the worker thread: bounded admission with
 * drop counting, batched draining and a close that flushes the remainder.
 */
class AiEvaluationExporterTest {

    private final List<AiEvaluationExportRecord> exported = new ArrayList<>();

    private final AiEvaluationSink sink = exported::addAll;

    private AiEvaluationExportRecord record(final String payload) {
        return new AiEvaluationExportRecord(AiEvaluationExportRecord.Type.SAMPLE, "eval-sample-v1", payload);
    }

    @Test
    void offersAreAdmittedAndDrainedInBatches() {
        AiEvaluationExporter exporter = new AiEvaluationExporter(8, 2, sink, false);
        exporter.offer(record("one"));
        exporter.offer(record("two"));
        exporter.offer(record("three"));

        assertEquals(3, exporter.getAccepted());
        assertEquals(0, exporter.getDropped());
        assertEquals(2, exporter.drainOnce());
        assertEquals(1, exporter.drainOnce());
        assertEquals(0, exporter.drainOnce());
        assertEquals(3, exported.size());
        assertEquals("one", exported.get(0).payloadJson());
        assertEquals(3, exporter.getAccepted());
        assertEquals(0, exporter.getDropped());
    }

    @Test
    void aFullQueueDropsWithoutBlockingAndCounts() {
        AiEvaluationExporter exporter = new AiEvaluationExporter(2, 4, sink, false);
        exporter.offer(record("one"));
        exporter.offer(record("two"));
        exporter.offer(record("three"));
        exporter.offer(record("four"));

        assertEquals(2, exporter.getAccepted());
        assertEquals(2, exporter.getDropped());
        assertEquals(2, exporter.drainOnce());
        assertEquals("one", exported.get(0).payloadJson());
        assertEquals("two", exported.get(1).payloadJson());
        assertTrue(exporter.queueIsEmpty());
    }

    @Test
    void closeFlushesTheRemainingBacklog() {
        AiEvaluationExporter exporter = new AiEvaluationExporter(8, 4, sink, false);
        exporter.offer(record("one"));
        exporter.offer(record("two"));

        exporter.close();

        assertEquals(2, exported.size());
        assertTrue(exporter.queueIsEmpty());
    }

    @Test
    void aFailingSinkDropsItsBatchWithoutBreakingTheDrain() {
        AiEvaluationExporter exporter = new AiEvaluationExporter(8, 4, batch -> {
            throw new IllegalStateException("sink down");
        }, false);
        exporter.offer(record("one"));
        exporter.offer(record("two"));

        assertEquals(2, exporter.drainOnce());
        assertTrue(exporter.queueIsEmpty());

        AiEvaluationExporter recovering = new AiEvaluationExporter(8, 4, sink, false);
        recovering.offer(record("after"));
        assertEquals(1, recovering.drainOnce());
        assertEquals(1, exported.size());
    }
}

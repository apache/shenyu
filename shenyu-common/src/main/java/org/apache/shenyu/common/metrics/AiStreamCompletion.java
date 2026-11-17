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

package org.apache.shenyu.common.metrics;

import java.util.Objects;

/**
 * Upstream completion evidence and downstream termination framing are independent facts.
 * Neither establishes business correctness, complete usage coverage or successful delivery.
 * @param evidence observed raw upstream evidence, never inferred from converted output
 * @param framing what the Chat encoder emitted
 */
public record AiStreamCompletion(Evidence evidence, Framing framing) {

    public static final AiStreamCompletion NOT_INSPECTED = new AiStreamCompletion(Evidence.NOT_INSPECTED, Framing.NOT_INSPECTED);

    public AiStreamCompletion {
        Objects.requireNonNull(evidence);
        Objects.requireNonNull(framing);
    }

    /** Closed, body-free raw-stream evidence. FINISH_REASON alone does not prove every requested choice finished. */
    public enum Evidence {
        NOT_INSPECTED, PENDING, UPSTREAM_MARKER, FINISH_REASON, UNKNOWN, INCOMPLETE, TRANSPORT_FAILURE, SOURCE_CANCELLED
    }

    /** Output framing says nothing about the origin or semantic correctness of the answer. */
    public enum Framing {
        NOT_INSPECTED, NONE, PASSTHROUGH_MARKER, SYNTHESIZED_MARKER
    }
}

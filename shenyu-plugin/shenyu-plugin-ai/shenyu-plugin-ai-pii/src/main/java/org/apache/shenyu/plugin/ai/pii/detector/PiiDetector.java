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

package org.apache.shenyu.plugin.ai.pii.detector;

import org.apache.shenyu.plugin.ai.pii.PiiMatch;

import java.util.List;

/**
 * A detector of one personally identifiable information type. Implementations
 * must be immutable and safe to call from concurrent requests: they hold no
 * per request state, every detection result is created for the call that
 * produced it.
 */
public interface PiiDetector {

    /**
     * The type reported by this detector, used as the action override key and
     * as the redaction placeholder name.
     *
     * @return the detector type
     */
    String type();

    /**
     * Detect every occurrence of the type inside the text.
     *
     * @param text the text to scan
     * @param deadlineNanos the absolute deadline, detectors that can overrun
     *                     must abort with {@link PiiDetectorTimeoutException}
     * @return the detections on the text, empty when nothing matched
     */
    List<PiiMatch> detect(CharSequence text, long deadlineNanos);
}

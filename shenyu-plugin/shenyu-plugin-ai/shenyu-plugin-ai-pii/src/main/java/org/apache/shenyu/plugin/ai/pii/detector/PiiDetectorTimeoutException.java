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

/**
 * Thrown when a detector exceeds its deadline, see
 * {@link PiiDetector#detect(CharSequence, long)}. The caller applies the
 * failure policy of the rule: a fail closed rule rejects the payload, a fail
 * open rule forwards it without the detections of the detector that overran.
 */
public class PiiDetectorTimeoutException extends RuntimeException {

    private static final long serialVersionUID = -1L;

    /**
     * A detector overran its deadline.
     *
     * @param type the detector type that overran
     */
    public PiiDetectorTimeoutException(final String type) {
        super("pii detector " + type + " exceeded its deadline");
    }
}

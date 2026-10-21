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

package org.apache.shenyu.web.diagnostics;

/**
 * Outcome of a single diagnostic section collection.
 *
 * <p>A bundle is deliberately partial-tolerant: every section carries an explicit
 * status so a consumer can tell a real observation from a failed, stale, truncated
 * or unsupported one instead of silently treating missing data as empty.</p>
 */
public enum SectionStatus {

    /**
     * The section was collected within its budget and is complete.
     */
    OK,

    /**
     * The section was collected but its content exceeded the size budget and was cut.
     */
    TRUNCATED,

    /**
     * The collector threw, timed out or was cancelled; no trustworthy content is present.
     */
    FAILED,

    /**
     * The observation exists but is older than the collection window.
     */
    STALE,

    /**
     * This gateway build or configuration does not support the section.
     */
    UNSUPPORTED,

    /**
     * The section was not attempted, for example because the total budget was exhausted
     * or the concurrent collection limit was reached.
     */
    SKIPPED
}

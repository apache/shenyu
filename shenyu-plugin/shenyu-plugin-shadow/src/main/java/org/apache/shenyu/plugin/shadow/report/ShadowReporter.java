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

package org.apache.shenyu.plugin.shadow.report;

import org.apache.shenyu.plugin.shadow.compare.ShadowComparison;
import org.apache.shenyu.plugin.shadow.compare.ShadowObservation;

/**
 * Receives bounded shadow comparison results.
 *
 * <p>Implementations must not depend on the primary exchange: they are called
 * from the shadow path, after the primary exchange terminated.</p>
 */
public interface ShadowReporter {

    /**
     * Report one comparison, including a failed shadow call.
     *
     * @param ruleId the rule cache key
     * @param comparison the bounded comparison result
     * @param primary the primary response observation
     * @param shadow the shadow response observation
     */
    void report(String ruleId, ShadowComparison comparison, ShadowObservation primary, ShadowObservation shadow);
}

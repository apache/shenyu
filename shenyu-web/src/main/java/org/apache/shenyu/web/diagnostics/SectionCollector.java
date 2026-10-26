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
 * A single, independently-budgeted contributor to a diagnostic bundle.
 *
 * <p>Implementations must return already-redacted content and must never throw for an
 * expected unsupported or empty condition; they should return a section with the matching
 * {@link SectionStatus} instead. Unexpected failures are captured by the orchestrator and
 * turned into a {@link SectionStatus#FAILED} section so one bad collector cannot sink the
 * whole bundle.</p>
 */
public interface SectionCollector {

    /**
     * The stable, filesystem-safe section name used as the archive entry key.
     *
     * @return the section name
     */
    String name();

    /**
     * Collect this section within the supplied budget.
     *
     * @param budget the independent time and size budget
     * @return the redacted section, never {@code null}
     */
    DiagnosticSection collect(DiagnosticBudget budget);
}

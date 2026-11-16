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

package org.apache.shenyu.plugin.ai.pii;

/**
 * One detection of a detector on a scanned text: the detector type together
 * with the half open range {@code [start, end)} of the matched value inside
 * that text. The matched value itself is intentionally not carried around,
 * so a match that ends up in a log never leaks the protected content.
 */
public final class PiiMatch {

    private final String type;

    private final int start;

    private final int end;

    /**
     * A detection of the given type on the range {@code [start, end)}.
     *
     * @param type the detector type
     * @param start the start offset, inclusive
     * @param end the end offset, exclusive
     */
    public PiiMatch(final String type, final int start, final int end) {
        this.type = type;
        this.start = start;
        this.end = end;
    }

    /**
     * get the detector type.
     *
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * get the start offset.
     *
     * @return the start offset, inclusive
     */
    public int getStart() {
        return start;
    }

    /**
     * get the end offset.
     *
     * @return the end offset, exclusive
     */
    public int getEnd() {
        return end;
    }
}

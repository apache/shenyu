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

package org.apache.shenyu.common.healthcheck;

import org.apache.shenyu.common.dto.convert.DivideUpstream;

import java.util.Objects;

public class UpstreamWithSelectorId {

    private String selectorId;

    private DivideUpstream divideUpstream;

    /**
     * no args constructor.
     */
    public UpstreamWithSelectorId() {
    }

    /**
     * all args constructor.
     *
     * @param selectorId     selectorId
     * @param divideUpstream divideUpstream
     */
    public UpstreamWithSelectorId(final String selectorId, final DivideUpstream divideUpstream) {
        this.selectorId = selectorId;
        this.divideUpstream = divideUpstream;
    }

    /**
     * get selectorId.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get divideUpstream.
     *
     * @return divideUpstream
     */
    public DivideUpstream getDivideUpstream() {
        return divideUpstream;
    }

    /**
     * set divideUpstream.
     *
     * @param divideUpstream divideUpstream
     */
    public void setDivideUpstream(final DivideUpstream divideUpstream) {
        this.divideUpstream = divideUpstream;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        UpstreamWithSelectorId that = (UpstreamWithSelectorId) o;
        return Objects.equals(selectorId, that.selectorId) && Objects.equals(divideUpstream, that.divideUpstream);
    }

    @Override
    public int hashCode() {
        return Objects.hash(selectorId, divideUpstream);
    }

    @Override
    public String toString() {
        return "UpstreamWithSelectorId{"
                + "selectorId='"
                + selectorId
                + '\''
                + ", divideUpstream="
                + divideUpstream
                + '}';
    }
}

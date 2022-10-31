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

package org.apache.shenyu.loadbalancer.cache;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Objects;

/**
 * The type Upstream with selector id.
 */
public class UpstreamWithSelectorId {

    private String selectorId;

    private Upstream upstream;

    /**
     * all args constructor.
     *
     * @param selectorId selectorId
     * @param upstream upstream
     */
    public UpstreamWithSelectorId(final String selectorId, final Upstream upstream) {
        this.selectorId = selectorId;
        this.upstream = upstream;
    }

    /**
     * get selectorId.
     *
     * @return selectorId selector id
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
     * get upstream.
     *
     * @return upstream upstream
     */
    public Upstream getUpstream() {
        return upstream;
    }

    /**
     * set upstream.
     *
     * @param upstream upstream
     */
    public void setUpstream(final Upstream upstream) {
        this.upstream = upstream;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        UpstreamWithSelectorId that = (UpstreamWithSelectorId) o;
        return Objects.equals(selectorId, that.selectorId) && Objects.equals(upstream, that.upstream);
    }

    @Override
    public int hashCode() {
        return Objects.hash(selectorId, upstream);
    }

    @Override
    public String toString() {
        return "UpstreamWithSelectorId{"
                + "selectorId='"
                + selectorId
                + '\''
                + ", upstream="
                + upstream
                + '}';
    }
}

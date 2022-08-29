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

package org.apache.shenyu.loadbalancer.entity;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * wrapper of upstream, including total weight and upstreams.
 **/
public class UpstreamHolder {

    /**
     * total weight for upstreams.
     */
    private int totalWeight;

    /**
     * upstreams.
     */
    private List<Upstream> upstreams;

    public UpstreamHolder() {
        this.totalWeight = 0;
        this.upstreams = new ArrayList<>();
    }

    /**
     * construct.
     *
     * @param totalWeight the total weight for upstreams.
     * @param upstreams the upstream.
     */
    public UpstreamHolder(final int totalWeight, final List<Upstream> upstreams) {
        this.totalWeight = totalWeight;
        this.upstreams = upstreams;
    }

    /**
     * get total weight.
     *
     * @return total weight.
     */
    public int getTotalWeight() {
        return totalWeight;
    }

    /**
     * set total weight.
     *
     * @param totalWeight total weight.
     */
    public void setTotalWeight(final int totalWeight) {
        this.totalWeight = totalWeight;
    }

    /**
     * get upstreams.
     *
     * @return the upstreams.
     */
    public List<Upstream> getUpstreams() {
        return upstreams;
    }

    /**
     * set upstreams.
     *
     * @param upstreams the upstreams.
     */
    public void setUpstreams(final List<Upstream> upstreams) {
        this.upstreams = upstreams;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        UpstreamHolder that = (UpstreamHolder) o;
        return totalWeight == that.totalWeight && Objects.equals(upstreams, that.upstreams);
    }

    @Override
    public int hashCode() {
        return Objects.hash(totalWeight, upstreams);
    }

    @Override
    public String toString() {
        return "UpstreamHolder{"
                + "totalWeight=" + totalWeight
                + ", upstreams=" + upstreams
                + '}';
    }

}

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

import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Objects;

/**
 * Immutable identity of a logical upstream target.
 */
public final class UpstreamIdentity {

    private final String protocol;

    private final String url;

    private final String group;

    private final String version;

    private final String hashKey;

    private final String leastActiveKey;

    UpstreamIdentity(final String protocol, final String url, final String group, final String version, final String domain) {
        this.protocol = protocol;
        this.url = url;
        this.group = group;
        this.version = version;
        boolean hasVariant = Objects.nonNull(group) || Objects.nonNull(version);
        this.hashKey = hasVariant ? GsonUtils.getInstance().toJson(new String[]{protocol, url, group, version}) : url;
        this.leastActiveKey = hasVariant ? GsonUtils.getInstance().toJson(new String[]{domain, group, version}) : domain;
    }

    /**
     * Get the hash ring key, preserving the address key for targets without a group or version.
     *
     * @return the hash ring key
     */
    public String hashKey() {
        return hashKey;
    }

    /**
     * Get the least active key, preserving the domain key for targets without a group or version.
     *
     * @return the least active key
     */
    public String leastActiveKey() {
        return leastActiveKey;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        UpstreamIdentity that = (UpstreamIdentity) o;
        return Objects.equals(protocol, that.protocol) && Objects.equals(url, that.url)
                && Objects.equals(group, that.group) && Objects.equals(version, that.version);
    }

    @Override
    public int hashCode() {
        return Objects.hash(protocol, url, group, version);
    }
}

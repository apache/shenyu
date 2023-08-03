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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;

import java.util.Objects;

/**
 * The type Dubbo rule handle.
 */
public class GrpcRuleHandle implements RuleHandle {

    /**
     * version.
     */
    private String version;

    /**
     * group.
     */
    private String group;

    /**
     * the loadbalance.
     * {@linkplain LoadBalanceEnum}
     */
    private String loadbalance = LoadBalanceEnum.RANDOM.getName();
    
    /**
     * New instance dubbo rule handle.
     *
     * @return the dubbo rule handle
     */
    public static GrpcRuleHandle newInstance() {
        return new GrpcRuleHandle();
    }
    
    /**
     * get version.
     *
     * @return version version
     */
    public String getVersion() {
        return version;
    }
    
    /**
     * set version.
     *
     * @param version version
     */
    public void setVersion(final String version) {
        this.version = version;
    }
    
    /**
     * get group.
     *
     * @return group group
     */
    public String getGroup() {
        return group;
    }
    
    /**
     * set group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }
    
    /**
     * Gets the value of loadbalance.
     *
     * @return the value of loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }
    
    /**
     * Sets the loadbalance.
     *
     * @param loadbalance loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GrpcRuleHandle)) {
            return false;
        }
        GrpcRuleHandle that = (GrpcRuleHandle) o;
        return Objects.equals(version, that.version)
                && Objects.equals(group, that.group)
                && Objects.equals(loadbalance, that.loadbalance);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version, group, loadbalance);
    }

    @Override
    public String toString() {
        return "DubboRuleHandle{"
                + "version='" + version + '\''
                + ", group='" + group + '\''
                + ", loadbalance='" + loadbalance + '\''
                + '}';
    }
}

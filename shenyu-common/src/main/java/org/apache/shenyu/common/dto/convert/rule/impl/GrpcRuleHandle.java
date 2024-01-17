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
 * The type Grpc rule handle.
 */
public class GrpcRuleHandle implements RuleHandle {

    /**
     * the loadBalance.
     * {@linkplain LoadBalanceEnum}
     */
    private String loadBalance = LoadBalanceEnum.RANDOM.getName();
    
    /**
     * New instance Grpc rule handle.
     *
     * @return the Grpc rule handle
     */
    public static GrpcRuleHandle newInstance() {
        return new GrpcRuleHandle();
    }
    
    /**
     * Gets the value of loadBalance.
     *
     * @return the value of loadBalance
     */
    public String getLoadBalance() {
        return loadBalance;
    }
    
    /**
     * Sets the loadBalance.
     *
     * @param loadBalance loadBalance
     */
    public void setLoadBalance(final String loadBalance) {
        this.loadBalance = loadBalance;
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
        return Objects.equals(loadBalance, that.loadBalance);
    }

    @Override
    public int hashCode() {
        return Objects.hash(loadBalance);
    }

    @Override
    public String toString() {
        return "GrpcRuleHandle{"
                + "loadBalance='" + loadBalance + '\''
                + '}';
    }
}

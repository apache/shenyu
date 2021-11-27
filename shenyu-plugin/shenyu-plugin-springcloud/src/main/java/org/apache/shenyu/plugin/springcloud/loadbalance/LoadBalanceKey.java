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

package org.apache.shenyu.plugin.springcloud.loadbalance;

import org.apache.shenyu.common.enums.LoadBalanceEnum;

/**
 * The load balance key.
 */
public class LoadBalanceKey {

    /**
     * ip.
     */
    private String ip;

    /**
     * selectorId.
     */
    private String selectorId;

    /**
     * loadBalance.
     * {@linkplain LoadBalanceEnum}
     */
    private String loadBalance;

    public LoadBalanceKey() {
    }

    public LoadBalanceKey(final String ip,
                          final String selectorId,
                          final String loadBalance) {
        this.ip = ip;
        this.selectorId = selectorId;
        this.loadBalance = loadBalance;
    }

    /**
     * get ip.
     *
     * @return ip ip
     */
    public String getIp() {
        return ip;
    }

    /**
     * set ip.
     *
     * @param ip ip
     */
    public void setIp(final String ip) {
        this.ip = ip;
    }

    /**
     * get loadBalance.
     *
     * @return loadBalance
     */
    public String getLoadBalance() {
        return loadBalance;
    }

    /**
     * set loadBalance.
     *
     * @param loadBalance loadBalance
     */
    public void setLoadBalance(final String loadBalance) {
        this.loadBalance = loadBalance;
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

}

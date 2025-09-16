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

package org.apache.shenyu.plugin.dubbo.common.cache;

public class DubboMethodParam {

    /**
     * name.
     */
    private String name;

    /**
     * loadbalance.
     */
    private String loadbalance;

    /**
     * retries.
     */
    private Integer retries;

    /**
     * timeout.
     */
    private Integer timeout;

    /**
     * sent.
     */
    private Boolean sent;

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get loadbalance.
     *
     * @return loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }

    /**
     * set loadbalance.
     *
     * @param loadbalance loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    /**
     * get retries.
     *
     * @return retries
     */
    public Integer getRetries() {
        return retries;
    }

    /**
     * set retries.
     *
     * @param retries retries
     */
    public void setRetries(final Integer retries) {
        this.retries = retries;
    }

    /**
     * get timeout.
     *
     * @return timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * set timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    /**
     * get sent.
     *
     * @return sent
     */
    public Boolean getSent() {
        return sent;
    }

    /**
     * set sent.
     *
     * @param sent sent
     */
    public void setSent(final Boolean sent) {
        this.sent = sent;
    }
}

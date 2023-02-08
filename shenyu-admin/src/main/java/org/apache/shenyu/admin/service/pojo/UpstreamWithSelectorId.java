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

package org.apache.shenyu.admin.service.pojo;

import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;

/**
 * check upstream selectorId pojo.
 */
public class UpstreamWithSelectorId {
    private String selectorId;

    private CommonUpstream upstream;

    /**
     * total check times.
     */
    private int zombieCheckTimes;

    public UpstreamWithSelectorId(final String selectorId, final CommonUpstream upstream) {
        this.selectorId = selectorId;
        this.upstream = upstream;
    }

    public UpstreamWithSelectorId(final String selectorId, final CommonUpstream upstream, final int zombieCheckTimes) {
        this.selectorId = selectorId;
        this.upstream = upstream;
        this.zombieCheckTimes = zombieCheckTimes;
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
     * get upstream.
     *
     * @return upstream
     */
    public CommonUpstream getUpstream() {
        return upstream;
    }

    /**
     * set upstream.
     *
     * @param upstream upstream
     */
    public void setUpstream(final CommonUpstream upstream) {
        this.upstream = upstream;
    }

    /**
     * get zombieCheckTimes.
     *
     * @return zombieCheckTimes
     */
    public int getZombieCheckTimes() {
        return zombieCheckTimes;
    }

    /**
     * set zombieCheckTimes.
     *
     * @param zombieCheckTimes zombieCheckTimes
     */
    public void setZombieCheckTimes(final int zombieCheckTimes) {
        this.zombieCheckTimes = zombieCheckTimes;
    }
}

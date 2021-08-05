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

package org.apache.shenyu.web.configuration.properties;


/**
 * The type shenyu config.
 */
public class ShenyuConfig {

    private Integer filterTime = 10;

    private Boolean filterTimeEnable = false;

    private Integer upstreamScheduledTime = 30;

    private Integer fileMaxSize = 10;

    /**
     * get filterTime.
     *
     * @return filterTime
     */
    public Integer getFilterTime() {
        return filterTime;
    }

    /**
     * set filterTime.
     *
     * @param filterTime filterTime.
     */
    public void setFilterTime(final Integer filterTime) {
        this.filterTime = filterTime;
    }

    /**
     * get filterTimeEnable.
     *
     * @return filterTimeEnable
     */
    public Boolean getFilterTimeEnable() {
        return filterTimeEnable;
    }

    /**
     * set filterTimeEnable.
     *
     * @param filterTimeEnable filterTimeEnable.
     */
    public void setFilterTimeEnable(final Boolean filterTimeEnable) {
        this.filterTimeEnable = filterTimeEnable;
    }

    /**
     * get upstreamScheduledTime.
     *
     * @return upstreamScheduledTime
     */
    public Integer getUpstreamScheduledTime() {
        return upstreamScheduledTime;
    }

    /**
     * set upstreamScheduledTime.
     *
     * @param upstreamScheduledTime upstreamScheduledTime.
     */
    public void setUpstreamScheduledTime(final Integer upstreamScheduledTime) {
        this.upstreamScheduledTime = upstreamScheduledTime;
    }

    /**
     * get fileMaxSize.
     *
     * @return fileMaxSize
     */
    public Integer getFileMaxSize() {
        return fileMaxSize;
    }

    /**
     * set fileMaxSize.
     *
     * @param fileMaxSize fileMaxSize.
     */
    public void setFileMaxSize(final Integer fileMaxSize) {
        this.fileMaxSize = fileMaxSize;
    }
}

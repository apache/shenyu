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

/**
 * DubboParam.
 */
public class DubboParam {

    private String group;

    private String version;

    private String loadbalance;

    private Integer retries;

    private Integer timeout;

    private String url;

    private Boolean sent;

    public String getGroup() {
        return group;
    }

    public void setGroup(final String group) {
        this.group = group;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(final String version) {
        this.version = version;
    }

    public String getLoadbalance() {
        return loadbalance;
    }

    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
    }

    public Integer getRetries() {
        return retries;
    }

    public void setRetries(final Integer retries) {
        this.retries = retries;
    }

    public Integer getTimeout() {
        return timeout;
    }

    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(final String url) {
        this.url = url;
    }

    public Boolean getSent() {
        return sent;
    }

    public void setSent(final Boolean sent) {
        this.sent = sent;
    }
}

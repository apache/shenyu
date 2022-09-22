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

package org.apache.shenyu.admin.model.vo;

import java.util.Collection;
import java.util.List;

/**
 * DocVO.
 */
public class DocVO {

    private String cookie;

    private String appKey;

    private String gatewayUrl;

    private List<EnvConfig> envProps;

    private Collection<MenuProject> menuProjects;

    /**
     * getGatewayUrl.
     *
     * @return String
     */
    public String getGatewayUrl() {
        return gatewayUrl;
    }

    /**
     * setGatewayUrl.
     *
     * @param gatewayUrl gatewayUrl
     */
    public void setGatewayUrl(final String gatewayUrl) {
        this.gatewayUrl = gatewayUrl;
    }


    /**
     * getEnvProps.
     * @return List
     */
    public List<EnvConfig> getEnvProps() {
        return envProps;
    }

    /**
     * setEnvProps.
     * @param envProps envProps
     */
    public void setEnvProps(final List<EnvConfig> envProps) {
        this.envProps = envProps;
    }

    /**
     * getCookie.
     *
     * @return String
     */
    public String getCookie() {
        return cookie;
    }

    /**
     * setCookie.
     *
     * @param cookie cookie
     */
    public void setCookie(final String cookie) {
        this.cookie = cookie;
    }

    /**
     * getAppKey.
     *
     * @return String
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * setAppKey.
     *
     * @param appKey appKey
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
    }

    /**
     * getMenuProjects.
     *
     * @return Collection
     */
    public Collection<MenuProject> getMenuProjects() {
        return menuProjects;
    }

    /**
     * setMenuProjects.
     *
     * @param menuProjects menuProjects
     */
    public void setMenuProjects(final Collection<MenuProject> menuProjects) {
        this.menuProjects = menuProjects;
    }

    /**
     * environment config.
     */
    public static class EnvConfig {

        private String envLabel;

        private String envDesc;

        private String addressUrl;

        /**
         * getEnvLabel.
         * @return String
         */
        public String getEnvLabel() {
            return envLabel;
        }

        /**
         * setEnvLabel.
         * @param envLabel envLabel
         */
        public void setEnvLabel(final String envLabel) {
            this.envLabel = envLabel;
        }

        /**
         * get environment description.
         * @return String
         */
        public String getEnvDesc() {
            return envDesc;
        }

        /**
         * set environment description.
         * @param envDesc envDesc
         */
        public void setEnvDesc(final String envDesc) {
            this.envDesc = envDesc;
        }

        /**
         * getAddressUrl.
         * @return String
         */
        public String getAddressUrl() {
            return addressUrl;
        }

        /**
         * setAddressUrl.
         * @param addressUrl addressUrl
         */
        public void setAddressUrl(final String addressUrl) {
            this.addressUrl = addressUrl;
        }
    }
}

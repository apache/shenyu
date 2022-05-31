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

package org.apache.shenyu.admin.config.properties;

import java.util.List;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * ApiDoc properties.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.apidoc")
public class ApiDocProperties {

    private String gatewayUrl;

    private List<EnvConfig> envProps;

    /**
     * getGatewayUrl.
     * @return String
     */
    public String getGatewayUrl() {
        return gatewayUrl;
    }

    /**
     * setGatewayUrl.
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
     * environment config.
     */
    public static class EnvConfig {

        private String envLabel;

        private String addressLabel;

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
         * getAddressLabel.
         * @return String
         */
        public String getAddressLabel() {
            return addressLabel;
        }

        /**
         * setAddressLabel.
         * @param addressLabel addressLabel
         */
        public void setAddressLabel(final String addressLabel) {
            this.addressLabel = addressLabel;
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

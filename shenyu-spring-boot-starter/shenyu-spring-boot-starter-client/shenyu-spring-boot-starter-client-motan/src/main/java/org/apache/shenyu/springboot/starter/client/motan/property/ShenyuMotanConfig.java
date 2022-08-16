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

package org.apache.shenyu.springboot.starter.client.motan.property;

/**
 * Shenyu motan properties.
 */
public class ShenyuMotanConfig {

    private Protocol protocol = new Protocol();

    private BasicServiceConfig basicServiceConfig = new BasicServiceConfig();

    /**
     * Get the protocol config.
     * @return protocol config
     */
    public Protocol getProtocol() {
        return protocol;
    }

    /**
     * Set the protocol config.
     * @param protocol protocol config
     */
    public void setProtocol(final Protocol protocol) {
        this.protocol = protocol;
    }

    /**
     * Get the BasicServiceConfig.
     * @return basicServiceConfig
     */
    public BasicServiceConfig getBasicServiceConfig() {
        return basicServiceConfig;
    }

    /**
     * Set the basicServiceConfig.
     * @param basicServiceConfig basicServiceConfig
     */
    public void setBasicServiceConfig(final BasicServiceConfig basicServiceConfig) {
        this.basicServiceConfig = basicServiceConfig;
    }

    public static class Protocol {

        private boolean isDefault = true;

        private String name = "motan2";

        private Integer maxContentLength = 1048576;

        /**
         * Get is default.
         * @return isDefault isDefault
         */
        public boolean isDefault() {
            return isDefault;
        }

        /**
         * Set is default.
         * @param isDefault isDefault
         */
        public void setDefault(final boolean isDefault) {
            this.isDefault = isDefault;
        }

        /**
         * Get the name.
         * @return name
         */
        public String getName() {
            return name;
        }

        /**
         * Set the name.
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * Get the maxContentLength.
         * @return maxContentLength
         */
        public Integer getMaxContentLength() {
            return maxContentLength;
        }

        /**
         * Set the maxContentLength.
         * @param maxContentLength maxContentLength
         */
        public void setMaxContentLength(final Integer maxContentLength) {
            this.maxContentLength = maxContentLength;
        }
    }

    public static class BasicServiceConfig {

        private String exportPort;

        private String group = "motan-shenyu-rpc";

        private boolean accessLog;

        private boolean shareChannel = true;

        private String module;

        private String application;

        private String registry = "registryConfig";

        private Integer requestTimeout = 2000;

        private boolean registryHeartBeatSwitcher = true;

        /**
         * Get the exportPort.
         * @return exportPort
         */
        public String getExportPort() {
            return exportPort;
        }

        /**
         * Set the exportPort.
         * @param exportPort exportPort
         */
        public void setExportPort(final String exportPort) {
            this.exportPort = exportPort;
        }

        /**
         * Get the group.
         * @return group
         */
        public String getGroup() {
            return group;
        }

        /**
         * Set the group.
         * @param group group
         */
        public void setGroup(final String group) {
            this.group = group;
        }

        /**
         * Get the accessLog.
         * @return accessLog
         */
        public boolean isAccessLog() {
            return accessLog;
        }

        /**
         * Set the accessLog.
         * @param accessLog accessLog
         */
        public void setAccessLog(final boolean accessLog) {
            this.accessLog = accessLog;
        }

        /**
         * Get the shareChannel.
         * @return shareChannel
         */
        public boolean isShareChannel() {
            return shareChannel;
        }

        /**
         * Set the shareChannel.
         * @param shareChannel shareChannel
         */
        public void setShareChannel(final boolean shareChannel) {
            this.shareChannel = shareChannel;
        }

        /**
         * Get the module.
         * @return module
         */
        public String getModule() {
            return module;
        }

        /**
         * Set the module.
         * @param module module
         */
        public void setModule(final String module) {
            this.module = module;
        }

        /**
         * Get the application.
         * @return application
         */
        public String getApplication() {
            return application;
        }

        /**
         * Set the application.
         * @param application application
         */
        public void setApplication(final String application) {
            this.application = application;
        }

        /**
         * Get the registry.
         * @return registry
         */
        public String getRegistry() {
            return registry;
        }

        /**
         * Set the registry.
         * @param registry registry
         */
        public void setRegistry(final String registry) {
            this.registry = registry;
        }

        /**
         * Get the requestTimeout.
         * @return requestTimeout
         */
        public Integer getRequestTimeout() {
            return requestTimeout;
        }

        /**
         * Set the requestTimeout.
         * @param requestTimeout requestTimeout
         */
        public void setRequestTimeout(final Integer requestTimeout) {
            this.requestTimeout = requestTimeout;
        }

        /**
         * Get the registryHeartBeatSwitcher.
         * @return registryHeartBeatSwitcher
         */
        public boolean getRegistryHeartBeatSwitcher() {
            return registryHeartBeatSwitcher;
        }

        /**
         * Set the registryHeartBeatSwitcher.
         * @param registryHeartBeatSwitcher registryHeartBeatSwitcher
         */
        public void setRegistryHeartBeatSwitcher(final boolean registryHeartBeatSwitcher) {
            this.registryHeartBeatSwitcher = registryHeartBeatSwitcher;
        }
    }
}

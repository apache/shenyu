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

package org.apache.shenyu.register.client.polaris.model;

import com.tencent.polaris.api.plugin.configuration.ConfigFile;
import com.tencent.polaris.plugins.circuitbreaker.common.ConfigGroup;

import java.util.Collection;

/**
 * ConfigFilesResponse.
 */
public class ConfigFilesResponse {

    private ConfigFilesResponse parent;

    private int code;

    private String info;

    private ConfigGroup group;

    private Collection<ConfigFile> files;

    /**
     * get group.
     * @return ConfigGroup
     */
    public ConfigGroup getGroup() {
        return group;
    }

    /**
     * get code.
     * @return int
     */
    public int getCode() {
        return code;
    }

    /**
     * get info.
     * @return String
     */
    public String getInfo() {
        return info;
    }

    /**
     * get files.
     * @return Collection
     */
    public Collection<ConfigFile> getFiles() {
        return files;
    }

    /**
     * get parent.
     * @return ConfigFilesResponse
     */
    public ConfigFilesResponse getParent() {
        return parent;
    }

    /**
     * set parent.
     * @param parent parent
     */
    public void setParent(final ConfigFilesResponse parent) {
        this.parent = parent;
    }

    /**
     * builder.
     * @return ConfigFilesResponse.Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "ConfigFilesResponse{" 
                + "parent=" + parent
                + ", code=" + code
                + ", info='" + info + '\''
                + ", group=" + group
                + ", files=" + files
                + '}';
    }

    public static final class Builder {
        
        private int code;
        
        private String info;
        
        private ConfigGroup group;
        
        private Collection<ConfigFile> files;

        private Builder() {
        }

        /**
         * code.
         * @param code code
         * @return Builder
         */
        public Builder code(final int code) {
            this.code = code;
            return this;
        }

        /**
         * info.
         * @param info info
         * @return Builder
         */
        public Builder info(final String info) {
            this.info = info;
            return this;
        }

        /**
         * group.
         * @param group group
         * @return Builder
         */
        public Builder group(final ConfigGroup group) {
            this.group = group;
            return this;
        }

        /**
         * files.
         * @param files files
         * @return Builder
         */
        public Builder files(final Collection<ConfigFile> files) {
            this.files = files;
            return this;
        }

        /**
         * build.
         * @return ConfigFilesResponse
         */
        public ConfigFilesResponse build() {
            ConfigFilesResponse configFilesResponse = new ConfigFilesResponse();
            configFilesResponse.code = this.code;
            configFilesResponse.files = this.files;
            configFilesResponse.info = this.info;
            configFilesResponse.group = this.group;
            return configFilesResponse;
        }
    }
    
}

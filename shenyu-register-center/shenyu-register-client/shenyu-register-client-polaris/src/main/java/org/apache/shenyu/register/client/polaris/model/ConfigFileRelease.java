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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ConfigFileRelease {

    @JsonProperty("id")
    private long id;

    @JsonProperty("name")
    private String name;

    @JsonProperty("namespace")
    private String namespace;

    @JsonProperty("group")
    private String group;

    @JsonProperty("file_name")
    private String fileName;

    @JsonProperty("content")
    private String content;

    @JsonProperty("md5")
    private String md5;

    @JsonProperty("version")
    private long version;

    @JsonProperty("modify_time")
    private Date modifyTime;

    @JsonIgnore
    private Map<String, String> labels = new HashMap<>();

    @JsonIgnore
    private boolean valid = true;

    /**
     * get id.
     *
     * @return Long
     */
    public Long getId() {
        return id;
    }

    /**
     * get name.
     *
     * @return String
     */
    public String getName() {
        return name;
    }

    /**
     * get namespace.
     *
     * @return String
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * get group.
     *
     * @return String
     */
    public String getGroup() {
        return group;
    }

    /**
     * get fileName.
     *
     * @return String
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * get content.
     *
     * @return String
     */
    public String getContent() {
        return content;
    }

    /**
     * get md5.
     *
     * @return String
     */
    public String getMd5() {
        return md5;
    }

    /**
     * get version.
     *
     * @return Long
     */
    public Long getVersion() {
        return version;
    }

    /**
     * get modifyTime.
     *
     * @return Date
     */
    public Date getModifyTime() {
        return modifyTime;
    }

    /**
     * get labels.
     *
     * @return Map
     */
    public Map<String, String> getLabels() {
        return labels;
    }

    /**
     * set labels.
     *
     * @param labels labels
     */
    public void setLabels(final Map<String, String> labels) {
        this.labels = labels;
    }

    /**
     * get keyInfo.
     *
     * @return String
     */
    public String keyInfo() {
        return String.format("%s@@%s@@%s", namespace, group, fileName);
    }

    /**
     * is valid.
     *
     * @return Boolean
     */
    public Boolean isValid() {
        return valid;
    }

    /**
     * equals.
     *
     * @param o o
     * @return boolean
     */
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ConfigFileRelease)) {
            return false;
        }
        ConfigFileRelease that = (ConfigFileRelease) o;
        return id == that.id && version == that.version && Objects.equals(namespace, that.namespace)
                && Objects.equals(group, that.group) && Objects.equals(fileName, that.fileName);
    }

    /**
     * hashCode.
     *
     * @return int
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, namespace, group, fileName, version);
    }

    @Override
    public String toString() {
        return "ConfigFileRelease{"
                + "id=" + id
                + ", name='" + name + '\''
                + ", namespace='" + namespace + '\''
                + ", group='" + group + '\''
                + ", fileName='" + fileName + '\''
                + ", content='" + content + '\''
                + ", md5='" + md5 + '\''
                + ", version=" + version
                + ", modifyTime=" + modifyTime
                + ", valid=" + valid
                + '}';
    }

    /**
     * builder api.
     *
     * @return ConfigFileRelease.Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        
        private long id;
        
        private String name;
        
        private String namespace;
        
        private String group;
        
        private String fileName;
        
        private String content;
        
        private String md5;
        
        private long version;
        
        private Date modifyTime;
        
        private Map<String, String> labels;
        
        private boolean valid;

        private Builder() {
        }

        /**
         * id.
         * @param id id
         * @return Builder
         */
        public Builder id(final Long id) {
            this.id = id;
            return this;
        }

        /**
         * name.
         * @param name name
         * @return Builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * namespace.
         * @param namespace namespace
         * @return Builder
         */
        public Builder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        /**
         * group.
         * @param group group
         * @return Builder
         */
        public Builder group(final String group) {
            this.group = group;
            return this;
        }

        /**
         * fileName.
         * @param fileName fileName
         * @return Builder
         */
        public Builder fileName(final String fileName) {
            this.fileName = fileName;
            return this;
        }

        /**
         * content.
         * @param content content
         * @return Builder
         */
        public Builder content(final String content) {
            this.content = content;
            return this;
        }

        /**
         * md5.
         * @param md5 md5
         * @return Builder
         */
        public Builder md5(final String md5) {
            this.md5 = md5;
            return this;
        }

        /**
         * version.
         * @param version version
         * @return Builder
         */
        public Builder version(final Long version) {
            this.version = version;
            return this;
        }

        /**
         * modifyTime.
         * @param modifyTime modifyTime
         * @return Builder
         */
        public Builder modifyTime(final Date modifyTime) {
            this.modifyTime = modifyTime;
            return this;
        }

        /**
         * labels.
         * @param labels labels
         * @return Builder
         */
        public Builder labels(final Map<String, String> labels) {
            this.labels = labels;
            return this;
        }

        /**
         * valid.
         * @param valid valid
         * @return Builder
         */
        public Builder valid(final Boolean valid) {
            this.valid = valid;
            return this;
        }

        /**
         * build.
         * @return ConfigFileRelease
         */
        public ConfigFileRelease build() {
            ConfigFileRelease configFileRelease = new ConfigFileRelease();
            configFileRelease.id = this.id;
            configFileRelease.labels = this.labels;
            configFileRelease.content = this.content;
            configFileRelease.valid = this.valid;
            configFileRelease.name = this.name;
            configFileRelease.group = this.group;
            configFileRelease.namespace = this.namespace;
            configFileRelease.version = this.version;
            configFileRelease.modifyTime = this.modifyTime;
            configFileRelease.fileName = this.fileName;
            configFileRelease.md5 = this.md5;
            return configFileRelease;
        }
    }
}

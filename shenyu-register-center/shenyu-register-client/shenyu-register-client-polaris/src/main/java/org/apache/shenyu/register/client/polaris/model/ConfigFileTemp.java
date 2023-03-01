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

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

/**
 * ConfigFileTemp.
 */
public class ConfigFileTemp {

    @JsonProperty("namespace")
    private String namespace;

    @JsonProperty("group")
    private String group;

    @JsonProperty("name")
    private String fileName;

    @JsonProperty("content")
    private String content;

    @JsonProperty("comment")
    private String comment;

    @JsonProperty("tags")
    private List<Tag> tags;

    @JsonProperty("format")
    private String format;

    /**
     * get namespace.
     *
     * @return String
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * set namespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
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
     * set group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
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
     * set fileName.
     *
     * @param fileName fileName
     */
    public void setFileName(final String fileName) {
        this.fileName = fileName;
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
     * set content.
     *
     * @param content content
     */
    public void setContent(final String content) {
        this.content = content;
    }

    /**
     * get comment.
     *
     * @return String
     */
    public String getComment() {
        return comment;
    }

    /**
     * set comment.
     *
     * @param comment comment
     */
    public void setComment(final String comment) {
        this.comment = comment;
    }

    /**
     * get tags.
     *
     * @return List
     */
    public List<Tag> getTags() {
        return tags;
    }

    /**
     * set tags.
     *
     * @param tags tags
     */
    public void setTags(final List<Tag> tags) {
        this.tags = tags;
    }

    /**
     * get format.
     *
     * @return String
     */
    public String getFormat() {
        return format;
    }

    /**
     * set format.
     *
     * @param format format
     */
    public void setFormat(final String format) {
        this.format = format;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ConfigFileTemp)) {
            return false;
        }
        ConfigFileTemp that = (ConfigFileTemp) o;
        return Objects.equals(getNamespace(), that.getNamespace()) && Objects.equals(getGroup(), that.getGroup())
                && Objects.equals(getFileName(), that.getFileName()) && Objects.equals(getContent(), that.getContent())
                && Objects.equals(getFormat(), that.getFormat());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getNamespace(), getGroup(), getFileName(), getContent(), getFormat());
    }

    @Override
    public String toString() {
        return "ConfigFileTemp{"
                + "namespace='" + namespace + '\''
                + ", group='" + group + '\''
                + ", fileName='" + fileName + '\''
                + ", content='" + content + '\''
                + ", comment='" + comment + '\''
                + ", tags=" + tags
                + ", format='" + format + '\''
                + '}';
    }

    /**
     * builder.
     * @return ConfigFileTempBuilder
     */
    public static ConfigFileTempBuilder builder() {
        return new ConfigFileTempBuilder();
    }

    public static class Tag {
        private String key;

        private String value;

        public Tag(final String key, final String value) {
            this.key = key;
            this.value = value;
        }

        /**
         * get key.
         * @return String
         */
        public String getKey() {
            return key;
        }

        /**
         * set key.
         * @param key key
         */
        public void setKey(final String key) {
            this.key = key;
        }

        /**
         * get key.
         * @return String
         */
        public String getValue() {
            return value;
        }

        /**
         * set value.
         * @param value value
         */
        public void setValue(final String value) {
            this.value = value;
        }
        
    }

    public static final class ConfigFileTempBuilder {
        
        private String namespace;
        
        private String group;
        
        private String fileName;
        
        private String content;
        
        private String comment;
        
        private List<Tag> tags;
        
        private String format;

        private ConfigFileTempBuilder() {
        }

        /**
         * namespace.
         * @param namespace namespace
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        /**
         * group.
         * @param group group
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder group(final String group) {
            this.group = group;
            return this;
        }

        /**
         * fileName.
         * @param fileName fileName
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder fileName(final String fileName) {
            this.fileName = fileName;
            return this;
        }

        /**
         * content.
         * @param content content
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder content(final String content) {
            this.content = content;
            return this;
        }

        /**
         * comment.
         * @param comment comment
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder comment(final String comment) {
            this.comment = comment;
            return this;
        }

        /**
         * tags.
         * @param tags tags
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder tags(final List<Tag> tags) {
            this.tags = tags;
            return this;
        }

        /**
         * format.
         * @param format format
         * @return ConfigFileTempBuilder
         */
        public ConfigFileTempBuilder format(final String format) {
            this.format = format;
            return this;
        }

        /**
         * build.
         * @return ConfigFileTemp
         */
        public ConfigFileTemp build() {
            ConfigFileTemp configFileTemp = new ConfigFileTemp();
            configFileTemp.setNamespace(namespace);
            configFileTemp.setGroup(group);
            configFileTemp.setFileName(fileName);
            configFileTemp.setContent(content);
            configFileTemp.setComment(comment);
            configFileTemp.setTags(tags);
            configFileTemp.setFormat(format);
            return configFileTemp;
        }
    }
    
}

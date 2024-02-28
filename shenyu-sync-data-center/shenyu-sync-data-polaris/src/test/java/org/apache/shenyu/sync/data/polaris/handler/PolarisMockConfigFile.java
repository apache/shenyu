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

package org.apache.shenyu.sync.data.polaris.handler;

import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeListener;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigKVFile;
import com.tencent.polaris.configuration.api.core.ConfigKVFileChangeListener;
import java.lang.reflect.Type;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;

public class PolarisMockConfigFile implements ConfigFile, ConfigKVFile {

    private final Properties properties = new Properties();

    private final String namespace;

    private final String fileGroup;

    private String fileName;

    private final String content;

    public PolarisMockConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        this(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName(), content);
    }

    public PolarisMockConfigFile(final String namespace, final String fileGroup, final String fileName) {
        this(namespace, fileGroup, fileName, null);
    }

    public PolarisMockConfigFile(final String namespace, final String fileGroup, final String fileName, final String content) {
        this.namespace = namespace;
        this.fileGroup = fileGroup;
        this.fileName = fileName;
        this.content = content;
    }

    @Override
    public String getContent() {
        return content;
    }

    @Override
    public <T> T asJson(final Class<T> aClass, final T t) {
        return null;
    }

    @Override
    public <T> T asJson(final Type type, final T t) {
        return null;
    }

    @Override
    public boolean hasContent() {
        return Objects.nonNull(content);
    }

    @Override
    public void addChangeListener(final ConfigFileChangeListener configFileChangeListener) {

    }

    @Override
    public void addChangeListener(final ConfigKVFileChangeListener listener) {

    }

    @Override
    public void removeChangeListener(final ConfigKVFileChangeListener listener) {

    }

    @Override
    public void removeChangeListener(final ConfigFileChangeListener configFileChangeListener) {

    }

    @Override
    public String getNamespace() {
        return namespace;
    }

    @Override
    public String getFileGroup() {
        return fileGroup;
    }

    /**
     * set file name.
     * @param fileName fileName
     */
    public void setFileName(final String fileName) {
        this.fileName = fileName;
    }

    @Override
    public String getFileName() {
        return fileName;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ConfigFileMetadata)) {
            return false;
        }
        final ConfigFileMetadata that = (ConfigFileMetadata) o;
        return Objects.equals(getNamespace(), that.getNamespace()) && Objects.equals(getFileGroup(), that.getFileGroup())
                   && Objects.equals(getFileName(), that.getFileName());
    }

    @Override
    public int hashCode() {
        return com.google.common.base.Objects.hashCode(getNamespace(), getFileGroup(), getFileName());
    }

    @Override
    public String getProperty(final String key, final String defaultValue) {
        return properties.getProperty(key, defaultValue);
    }

    @Override
    public Integer getIntProperty(final String key, final Integer defaultValue) {
        return Integer.valueOf(getProperty(key, Integer.toString(defaultValue)));
    }

    @Override
    public Long getLongProperty(final String key, final Long defaultValue) {
        return Long.valueOf(getProperty(key, Long.toString(defaultValue)));
    }

    @Override
    public Short getShortProperty(final String key, final Short defaultValue) {
        return Short.valueOf(getProperty(key, Short.toString(defaultValue)));
    }

    @Override
    public Float getFloatProperty(final String key, final Float defaultValue) {
        return Float.valueOf(getProperty(key, Float.toString(defaultValue)));
    }

    @Override
    public Double getDoubleProperty(final String key, final Double defaultValue) {
        return Double.valueOf(getProperty(key, Double.toString(defaultValue)));
    }

    @Override
    public Byte getByteProperty(final String key, final Byte defaultValue) {
        return Byte.valueOf(getProperty(key, Byte.toString(defaultValue)));
    }

    @Override
    public Boolean getBooleanProperty(final String key, final Boolean defaultValue) {
        return Boolean.valueOf(getProperty(key, Boolean.toString(defaultValue)));
    }

    @Override
    public String[] getArrayProperty(final String key, final String delimiter, final String[] defaultValue) {
        return new String[0];
    }

    @Override
    public <T extends Enum<T>> T getEnumProperty(final String key, final Class<T> enumType, final T defaultValue) {
        return null;
    }

    @Override
    public <T> T getJsonProperty(final String key, final Class<T> clazz, final T defaultValue) {
        return null;
    }

    @Override
    public <T> T getJsonProperty(final String key, final Type typeOfT, final T defaultValue) {
        return null;
    }

    @Override
    public Set<String> getPropertyNames() {
        return null;
    }

}

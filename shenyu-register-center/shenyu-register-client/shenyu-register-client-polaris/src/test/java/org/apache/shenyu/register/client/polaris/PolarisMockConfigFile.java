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

package org.apache.shenyu.register.client.polaris;

import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeListener;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import java.lang.reflect.Type;
import java.util.Objects;

public class PolarisMockConfigFile implements ConfigFile {

    private final String namespace;

    private final String fileGroup;

    private String fileName;

    private String content;

    public PolarisMockConfigFile(final ConfigFileMetadata configFileMetadata) {
        this(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName());
    }

    public PolarisMockConfigFile(final ConfigFileMetadata configFileMetadata, final String content) {
        this(configFileMetadata.getNamespace(), configFileMetadata.getFileGroup(), configFileMetadata.getFileName(), content);
    }

    public PolarisMockConfigFile(final String namespace, final String fileGroup, final String fileName) {
        this.namespace = namespace;
        this.fileGroup = fileGroup;
        this.fileName = fileName;
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
}

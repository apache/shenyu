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

import java.lang.reflect.Type;

public class PolarisMockConfigFile implements ConfigFile {

    private String content;

    public PolarisMockConfigFile(final String content) {
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
        return false;
    }

    @Override
    public void addChangeListener(final ConfigFileChangeListener configFileChangeListener) {

    }

    @Override
    public void removeChangeListener(final ConfigFileChangeListener configFileChangeListener) {

    }

    @Override
    public String getNamespace() {
        return null;
    }

    @Override
    public String getFileGroup() {
        return null;
    }

    @Override
    public String getFileName() {
        return null;
    }
}

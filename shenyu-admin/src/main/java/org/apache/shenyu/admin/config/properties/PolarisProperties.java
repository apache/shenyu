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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * The type Polaris config.
 */
@ConfigurationProperties(prefix = "shenyu.sync.polaris")
public class PolarisProperties {

    private String url;

    private String namespace;

    private String fileGroup;

    /**
     * Gets the value of url.
     *
     * @return the value of url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets the value of namespace.
     *
     * @return the value of namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * Sets the namespace.
     *
     * @param namespace namespace
     */
    public void setNamespace(final String namespace) {
        this.namespace = StringUtils.isNotBlank(namespace) ? namespace : PolarisPathConstants.NAMESPACE;
    }

    /**
     * Gets the value of fileGroup.
     *
     * @return the value of fileGroup.
     */
    public String getFileGroup() {
        return fileGroup;
    }

    /**
     * Sets the value of fileGroup.
     *
     * @param fileGroup fileGroup
     */
    public void setFileGroup(final String fileGroup) {
        this.fileGroup = StringUtils.isNotBlank(fileGroup) ? fileGroup : PolarisPathConstants.FILE_GROUP;
    }
}

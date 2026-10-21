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

package org.apache.shenyu.register.common.dto;

import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

/**
 * mcpTools registerDto.
 */
public class McpToolsRegisterDTO implements DataTypeParent {

    private MetaDataRegisterDTO metaDataRegisterDTO;

    private String namespaceId;

    private String mcpConfig;

    public MetaDataRegisterDTO getMetaDataRegisterDTO() {
        return metaDataRegisterDTO;
    }

    public void setMetaDataRegisterDTO(final MetaDataRegisterDTO metaDataRegisterDTO) {
        this.metaDataRegisterDTO = metaDataRegisterDTO;
    }

    public String getMcpConfig() {
        return mcpConfig;
    }

    public void setMcpConfig(final String mcpConfig) {
        this.mcpConfig = mcpConfig;
    }

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    @Override
    public DataType getType() {
        return DataType.MCP_TOOLS;
    }
}

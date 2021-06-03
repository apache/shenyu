/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.apache.shenyu.register.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.EqualsAndHashCode;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.io.Serializable;

/**
 * The type URI register dto.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class URIRegisterDTO implements DataTypeParent, Serializable {

    private static final long serialVersionUID = -653255079042814293L;

    private String appName;

    private String contextPath;

    private String rpcType;

    private String host;

    private Integer port;

    private EventType eventType;

    /**
     * Trans form uri register dto.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the uri register dto
     */
    public static URIRegisterDTO transForm(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return URIRegisterDTO.builder()
                .appName(metaDataRegisterDTO.getAppName())
                .contextPath(metaDataRegisterDTO.getContextPath())
                .rpcType(metaDataRegisterDTO.getRpcType())
                .host(metaDataRegisterDTO.getHost())
                .port(metaDataRegisterDTO.getPort()).build();
    }

    @Override
    public DataType getType() {
        return DataType.URI;
    }
}

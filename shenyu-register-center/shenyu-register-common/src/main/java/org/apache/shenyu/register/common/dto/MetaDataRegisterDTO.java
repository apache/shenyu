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
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.io.Serializable;
import java.util.List;

/**
 * The type Meta data dto.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MetaDataRegisterDTO implements DataTypeParent, Serializable {

    private static final long serialVersionUID = -9179185443399391316L;

    private String appName;

    private String contextPath;

    private String path;

    private String pathDesc;

    private String rpcType;

    private String serviceName;

    private String methodName;

    private String ruleName;

    private String parameterTypes;

    private String rpcExt;

    private boolean enabled;

    private String host;

    private Integer port;

    private List<String> pluginNames;

    private boolean registerMetaData;

    @Override
    public DataType getType() {
        return DataType.META_DATA;
    }
}

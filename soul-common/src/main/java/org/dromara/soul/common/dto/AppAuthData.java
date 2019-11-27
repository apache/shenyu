/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.common.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * AppAuthDTO.
 *
 * @author xiaoyu(Myth)
 * @author huangxiaofeng
 * @since 2.0.0
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AppAuthData implements Serializable {

    private String appKey;

    private String appSecret;

    private Boolean enabled;

    private List<AuthParamData> paramDataList;

    private List<AuthPathData> pathDataList;
}

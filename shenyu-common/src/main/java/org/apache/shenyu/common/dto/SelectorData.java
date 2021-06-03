/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.apache.shenyu.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

/**
 * SelectorDTO.
 *
 * @since 2.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SelectorData {

    private String id;

    private String pluginId;

    /**
     * plugin name.
     */
    private String pluginName;

    private String name;

    /**
     * matchMode（0 and  1 or).
     */
    private Integer matchMode;

    /**
     * type（false full，true custom).
     */
    private Integer type;

    private Integer sort;

    private Boolean enabled;

    private Boolean logged;

    private Boolean continued;

    private String handle;

    private List<ConditionData> conditionList;
}

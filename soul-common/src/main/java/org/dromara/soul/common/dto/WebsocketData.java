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

package org.dromara.soul.common.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * Data set, including {@link AppAuthData}、{@link ConditionData}、{@link PluginData}、{@link RuleData}、{@link SelectorData}.
 *
 * @param <T> the type parameter
 * @author xiaoyu
 * @since 2.0.0
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class WebsocketData<T> implements Serializable {

    private static final long serialVersionUID = -3068033915566306050L;

    /**
     * group type.
     * {@linkplain org.dromara.soul.common.enums.ConfigGroupEnum}
     */
    private String groupType;

    /**
     * event type.
     * {@linkplain org.dromara.soul.common.enums.DataEventTypeEnum}
     */
    private String eventType;

    /**
     * data list.
     * {@link AppAuthData}、{@link ConditionData}、{@link PluginData}、{@link RuleData}、{@link SelectorData}.
     */
    private List<T> data;

}

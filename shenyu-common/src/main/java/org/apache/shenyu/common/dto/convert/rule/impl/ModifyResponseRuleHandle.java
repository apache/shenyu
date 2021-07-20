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

package org.apache.shenyu.common.dto.convert.rule.impl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The type ModifyResponse rule handle.
 */
@ToString
@Getter
@Setter
@NoArgsConstructor
public class ModifyResponseRuleHandle implements RuleHandle {

    private static final long serialVersionUID = -3863724068338111444L;

    /**
     * add header map.
     */
    private Map<String, String> addHeaders;

    /**
     * set header map.
     */
    private Map<String, String> setHeaders;

    /**
     * replace header map
     * key: oldHeaderKey, value: newHeaderKey.
     */
    private Map<String, String> replaceHeaderKeys;

    /**
     * remove header List.
     */
    private Set<String> removeHeaderKeys;

    /**
     * http response status code.
     */
    private int statusCode;

    /**
     * add body List.
     */
    private List<ParamMappingHandle.ParamMapInfo> addBodyKeys;

    /**
     * replace body List.
     */
    private List<ParamMappingHandle.ParamMapInfo> replaceBodyKeys;

    /**
     * remove body List.
     */
    private Set<String> removeBodyKeys;

    @Override
    public RuleHandle createDefault(final String path) {
        this.statusCode = HttpStatus.OK.value();
        return this;
    }
}

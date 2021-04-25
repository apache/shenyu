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

package org.dromara.soul.common.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.dromara.soul.common.exception.SoulException;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * OperatorEnum.
 *
 * @author xiaoyu(Myth)
 */
@RequiredArgsConstructor
@Getter
public enum OperatorEnum {

    /**
     * Match operator enum.
     */
    MATCH("match", true),

    /**
     * Eq operator enum.
     */
    EQ("=", true),

    /**
     * Regex operator enum.
     */
    REGEX("regEx", true),

    /**
     * Gt operator enum.
     */
    GT(">", false),

    /**
     * Lt operator enum.
     */
    LT("<", false),

    /**
     * Like operator enum.
     */
    LIKE("like", true),
    
    /**
     * SpEL enum.
     */
    SPEL("SpEL", true);
    
    private final String alias;

    private final Boolean support;

    /**
     * acquire operator supports.
     *
     * @return operator support.
     */
    public static List<OperatorEnum> acquireSupport() {
        return Arrays.stream(OperatorEnum.values())
                .filter(e -> e.support).collect(Collectors.toList());
    }

    /**
     * get operator enum by alias.
     *
     * @param alias operator alias.
     * @return operator enum.
     */
    public static OperatorEnum getOperatorEnumByAlias(final String alias) {
        return Arrays.stream(OperatorEnum.values())
                .filter(e -> e.getAlias().equals(alias) && e.support).findFirst()
                .orElseThrow(() -> new SoulException(String.format(" this  operator can not support %s ", alias)));

    }
}

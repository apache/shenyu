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

package org.apache.shenyu.common.enums;

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * OperatorEnum.
 */
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
    REGEX("regex", true),

    /**
     * Gt operator enum.
     */
    GT(">", false),

    /**
     * Lt operator enum.
     */
    LT("<", false),

    /**
     * Contains operator enum.
     */
    CONTAINS("contains", true),

    /**
     * SpEL enum.
     */
    SPEL("SpEL", true),

    /**
     * Groovy enum.
     */
    GROOVY("Groovy", true),

    /**
     * Time before operator enum.
     */
    TIME_BEFORE("TimeBefore", true),

    /**
     * Time after operator enum.
     */
    TIME_AFTER("TimeAfter", true);

    private final String alias;

    private final Boolean support;

    /**
     * all args constructor.
     * @param alias alias
     * @param support support
     */
    OperatorEnum(final String alias, final Boolean support) {
        this.alias = alias;
        this.support = support;
    }

    /**
     * get alias.
     *
     * @return alias
     */
    public String getAlias() {
        return alias;
    }

    /**
     * get support.
     *
     * @return support
     */
    public Boolean getSupport() {
        return support;
    }

    /**
     * acquire operator supports.
     *
     * @return operator support.
     */
    public static List<OperatorEnum> acquireSupport() {
        return Arrays.stream(OperatorEnum.values()).filter(e -> e.support).collect(Collectors.toList());
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
                .orElseThrow(() -> new ShenyuException(String.format(" this  operator can not support %s ", alias)));
    }
}

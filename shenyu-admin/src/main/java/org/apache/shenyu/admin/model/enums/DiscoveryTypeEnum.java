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

package org.apache.shenyu.admin.model.enums;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * DiscoveryTypeEnum.
 */
public enum DiscoveryTypeEnum {

    /**
     * local.
     */
    LOCAL("local"),

    /**
     * zookeeper.
     */
    ZOOKEEPER("zookeeper"),

    /**
     * nacos.
     */
    NACOS("nacos"),

    /**
     * etcd.
     */
    ETCD("etcd"),

    /**
     * eureka.
     */
    EUREKA("eureka");

    /**
     * Discovery type.
     */
    private final String type;

    /**
     * constructor of DiscoveryTypeEnum.
     *
     * @param type discovery type
     */
    DiscoveryTypeEnum(final String type) {
        this.type = type;
    }

    /**
     * get discovery type.
     *
     * @return discovery type
     */
    public String getType() {
        return type;
    }

    /**
     * discovery type enum convert list.
     *
     * @return discovery type list
     */
    public static List<String> types() {
        return Stream.of(DiscoveryTypeEnum.values())
                .map(DiscoveryTypeEnum::getType)
                .collect(Collectors.toList());
    }
}

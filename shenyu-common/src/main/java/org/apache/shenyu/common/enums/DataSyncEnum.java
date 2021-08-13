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

import java.util.Arrays;

/**
 * The enum Data sync enum.
 */
public enum DataSyncEnum {

    /**
     * Http data sync enum.
     */
    HTTP("http"),

    /**
     * Zookeeper data sync enum.
     */
    ZOOKEEPER("zookeeper"),

    /**
     * Websocket data sync enum.
     */
    WEBSOCKET("websocket");

    private final String name;

    /**
     * all args constructor.
     *
     * @param name name
     */
    DataSyncEnum(final String name) {
        this.name = name;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Acquire by name data sync enum.
     *
     * @param name the name
     * @return the data sync enum
     */
    public static DataSyncEnum acquireByName(final String name) {
        return Arrays.stream(DataSyncEnum.values())
                .filter(e -> e.getName().equals(name)).findFirst()
                .orElse(DataSyncEnum.HTTP);
    }
}

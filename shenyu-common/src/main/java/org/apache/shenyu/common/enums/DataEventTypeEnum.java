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
import java.util.Objects;

/**
 * The enum Data event type.
 */
public enum DataEventTypeEnum {

    /**
     * delete event.
     */
    DELETE,

    /**
     * insert event.
     */
    CREATE,

    /**
     * update event.
     */
    UPDATE,

    /**
     * REFRESH data event type enum.
     */
    REFRESH,

    /**
     * Myself data event type enum.
     */
    MYSELF;

    /**
     * Acquire by name data event type enum.
     *
     * @param name the name
     * @return the data event type enum
     */
    public static DataEventTypeEnum acquireByName(final String name) {
        return Arrays.stream(DataEventTypeEnum.values())
                .filter(e -> Objects.equals(e.name(), name))
                .findFirst()
                .orElseThrow(() -> new ShenyuException(String.format(" this DataEventTypeEnum can not support %s", name)));
    }
}

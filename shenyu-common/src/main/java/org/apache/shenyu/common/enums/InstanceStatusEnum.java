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

public enum InstanceStatusEnum {

    /**
     * DELETED.
     */
    DELETED(0, "DELETED"),

    /**
     * ONLINE.
     */
    ONLINE(1, "ONLINE"),

    /**
     * OFFLINE.
     */
    OFFLINE(2, "OFFLINE"),

    /**
     * INITIALIZING.
     */
    INITIALIZING(3, "initializing"),

    /**
     * READY.
     */
    READY(4, "ready"),

    /**
     * READINESS_FAILED.
     */
    READINESS_FAILED(5, "readiness_failed"),

    /**
     * DRAINING.
     */
    DRAINING(6, "draining"),

    /**
     * UNKNOWN.
     */
    UNKNOWN(7, "unknown");

    private final int code;

    private final String name;

    InstanceStatusEnum(final int code, final String name) {
        this.code = code;
        this.name = name;
    }

    /**
     * get code.
     *
     * @return code
     */
    public int getCode() {
        return code;
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
     * get name by code.
     *
     * @param code code
     * @return name
     */
    public static String getNameByCode(final int code) {
        for (InstanceStatusEnum status : values()) {
            if (status.getCode() == code) {
                return status.getName();
            }
        }
        return DELETED.name;
    }

    /**
     * from code.
     *
     * @param code code
     * @return instance status enum
     */
    public static InstanceStatusEnum fromCode(final int code) {
        for (InstanceStatusEnum status : values()) {
            if (status.getCode() == code) {
                return status;
            }
        }
        return UNKNOWN;
    }

    /**
     * from name.
     *
     * @param name name
     * @return instance status enum
     */
    public static InstanceStatusEnum fromName(final String name) {
        for (InstanceStatusEnum status : values()) {
            if (status.getName().equalsIgnoreCase(name) || status.name().equalsIgnoreCase(name)) {
                return status;
            }
        }
        return UNKNOWN;
    }
}

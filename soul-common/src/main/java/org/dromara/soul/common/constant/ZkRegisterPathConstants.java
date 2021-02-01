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

package org.dromara.soul.common.constant;

/**
 * @author lw1243925457
 */
public class ZkRegisterPathConstants {

    /**
     * root path of zookeeper register center
     */
    public static final String ROOT_PATH = "/soulRegisterCenter";

    private static final String SEPARATOR = "/";

    public static String buildTypePath(final String child) {
        return String.join(SEPARATOR, ROOT_PATH, child);
    }

    public static String buildDataPath(final String parent,final String child) {
        return String.join(SEPARATOR, parent, child);
    }
}

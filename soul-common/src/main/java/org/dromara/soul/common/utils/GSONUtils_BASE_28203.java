/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.utils;

import com.google.gson.Gson;

/**
 * GSONUtils.
 * @author xiaoyu(Myth)
 */
public class GSONUtils {

    private static final GSONUtils INSTANCE = new GSONUtils();

    private static final Gson GSON = new Gson();

    public static GSONUtils getInstance() {
        return INSTANCE;
    }

    public String toJson(final Object object) {
        return GSON.toJson(object);
    }

    public <T> T fromJson(final String json, final Class<T> tClass) {
        return GSON.fromJson(json, tClass);
    }
}

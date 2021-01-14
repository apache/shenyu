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

package org.dromara.soul.web.dubbo;

import java.util.Map;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.dubbo.DubboParamResolveService;

/**
 * The type Default generic param resolve service.
 *
 * @author xiaoyu
 */
public class DefaultDubboParamResolveService implements DubboParamResolveService {

    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        final Map<String, Object> paramMap = GsonUtils.getInstance().toObjectMap(body);
        for (String key : paramMap.keySet()) {
            Object obj = paramMap.get(key);
            if (obj instanceof JsonObject) {
                paramMap.put(key, GsonUtils.getInstance().convertToMap(obj.toString()));
            } else if (obj instanceof JsonArray) {
                paramMap.put(key, GsonUtils.getInstance().fromList(obj.toString(), Object.class));
            } else {
                paramMap.put(key, obj);
            }
        }
        return new ImmutablePair<>(new String[]{parameterTypes}, new Object[]{paramMap});
    }
}

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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.dubbo.DubboParamResolveService;

/**
 * Dubbo multi parameter resolve service impl.
 */
@Slf4j
public class DubboMultiParameterResolveServiceImpl implements DubboParamResolveService {

    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        Map<String, Object> paramMap = GsonUtils.getInstance().toObjectMap(body);
        String[] parameter = StringUtils.split(parameterTypes, ",");
        if (parameter.length == 1 && !isBaseType(parameter[0])) {
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
            return new ImmutablePair<>(parameter, new Object[]{paramMap});
        }
        List<Object> list = new LinkedList<>();
        for (String key : paramMap.keySet()) {
            Object obj = paramMap.get(key);
            if (obj instanceof JsonObject) {
                list.add(GsonUtils.getInstance().convertToMap(obj.toString()));
            } else if (obj instanceof JsonArray) {
                list.add(GsonUtils.getInstance().fromList(obj.toString(), Object.class));
            } else {
                list.add(obj);
            }
        }
        Object[] objects = list.toArray();
        return new ImmutablePair<>(parameter, objects);
    }

    private boolean isBaseType(final String paramType) {
        return paramType.startsWith("java") || paramType.startsWith("[Ljava");
    }
}

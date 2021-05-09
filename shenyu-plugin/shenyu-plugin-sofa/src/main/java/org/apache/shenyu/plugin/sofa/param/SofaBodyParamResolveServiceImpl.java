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

package org.apache.shenyu.plugin.sofa.param;

import com.alipay.hessian.generic.model.GenericObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.param.BodyParamResolveService;
import org.apache.shenyu.plugin.api.utils.BodyParamUtils;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type Default generic param resolve service.
 */
public class SofaBodyParamResolveServiceImpl implements BodyParamResolveService {

    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        String[] parameterTypesAndGeneric = StringUtils.split(parameterTypes, "#");
        String[] parameters = StringUtils.split(parameterTypesAndGeneric[0], ",");
        if (isSingleCustomizeType(parameters)) {
            return BodyParamUtils.buildSingleParameter(body, parameterTypes);
        }
        LinkedList<String> genericTypes = new LinkedList<>();
        if (parameterTypesAndGeneric.length > 1) {
            genericTypes.addAll(Arrays.asList(StringUtils.split(parameterTypesAndGeneric[1], ",")));
        }
        Map<String, Object> paramMap = GsonUtils.getInstance().toObjectMap(body);
        Object[] objects = paramMap.values().stream().map(each -> {
            if (each instanceof JsonObject) {
                return GsonUtils.getInstance().convertToMap(each.toString());
            } else if (each instanceof JsonArray) {
                if (genericTypes.isEmpty()) {
                    return GsonUtils.getInstance().fromList(each.toString(), Object.class);
                }
                String type = genericTypes.pop();
                return convertToGenericObjects(type, each);
            } else {
                return each;
            }
        }).toArray();
        return new ImmutablePair<>(parameters, objects);
    }

    /**
     * Convert to GenericObject.
     *
     * @param type generic type.
     * @param param actual parameter.
     * @return list of GenericObject.
     */
    @SuppressWarnings("all")
    private static List<GenericObject> convertToGenericObjects(final String type, final Object param) {
        List<Map> mapList = GsonUtils.getInstance().fromList(param.toString(), Map.class);
        return mapList.stream().map(map -> {
            GenericObject genericObject = new GenericObject(type);
            map.forEach((key, value) -> genericObject.putField((String) key, value));
            return genericObject;
        }).collect(Collectors.toList());
    }

    /**
     * only one parameter which is customized type.
     *
     * @param parameter parameter array.
     * @return only one parameter and it's customized type return true otherwise return false.
     */
    private static boolean isSingleCustomizeType(final String[] parameter) {
        return parameter.length == 1 && !parameter[0].startsWith("java") && !parameter[0].startsWith("[Ljava");
    }
}

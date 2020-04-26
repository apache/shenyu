/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.bootstrap.dubbo;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.web.plugin.dubbo.GenericParamResolveService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Dubbo multi parameter  resolve service impl.
 *
 * @author xiaoyu
 */
public class DubboMultiParameterResolveServiceImpl implements GenericParamResolveService {

    private final Logger log = LoggerFactory.getLogger(getClass());
    /**
     * ObjectMapper is a completely thread-safe service class, it is meant to be used as singleton across the lifetime of the application. It is also very expensive
     * to create. ... It is also prudent to do a review of all usages of ObjectMapper and ensure that it is not instantiated every time. This is a performance killer
     */
    private final ObjectMapper om = new ObjectMapper();

    @SuppressWarnings("unchecked")
    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        String[] types = splitParameterTypes(parameterTypes);
        Object[] params = new Object[types.length];
        if (types.length > 0) {
            try {
                Object o = om.readValue(body, Object.class);
                if (o instanceof List) {
                    ((List<Object>) o).stream().limit(types.length).collect(Collectors.toList()).toArray(params);
                } else {
                    params[0] = o;
                }
            } catch (Exception e) {
                log.warn("body[{}] parsing failed cause by[{}]", body, e.getMessage());
            }
        }
        return new ImmutablePair<>(types, params);
    }

    private static String[] splitParameterTypes(String parameterTypes) {
        if (parameterTypes.length() == 0) {
            return new String[0];
        }
        List<String> ls = new ArrayList<>();
        int pL = parameterTypes.length();
        int i = 0, j = -1;
        for (; i < pL; i++) {
            if (parameterTypes.charAt(i) == ',' && i > j) {
                if (i != j + 1) {
                    ls.add(parameterTypes.substring(j + 1, i));
                }
                j = i;
            }
        }
        if (i > j) {
            if (i != j + 1) {
                ls.add(parameterTypes.substring(j + 1, i));
            }
        }
        String[] ss = new String[ls.size()];
        ls.toArray(ss);
        return ss;
    }
}

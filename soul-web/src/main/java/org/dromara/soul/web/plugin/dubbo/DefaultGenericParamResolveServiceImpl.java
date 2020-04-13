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

package org.dromara.soul.web.plugin.dubbo;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The type Default generic param resolve service.
 *
 * @author xiaoyu
 */
public class DefaultGenericParamResolveServiceImpl implements GenericParamResolveService {
    private Logger log = LoggerFactory.getLogger(getClass());
    
    @SuppressWarnings("unchecked")
    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        ObjectMapper om = new ObjectMapper();
        String[] ptypes = splitParameterTypes(parameterTypes);
        Object[] params = new Object[ptypes.length];
        if (ptypes.length > 0) {
            try {
                Object o = om.readValue(body, Object.class);
                if (o instanceof List) {
                    ((List<Object>) o)
                            .stream()
                            .limit(ptypes.length)
                            .collect(Collectors.toList())
                            .toArray(params);
                } else {
                    params[0] = o;
                }
            } catch (Exception e) {
                log.warn("body[{}] parsing failed cause by[{}]", body, e.getMessage());
            }
        }
        return new ImmutablePair<>(ptypes, params);
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

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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.ClassUtils;
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

    private static final Logger LOG = LoggerFactory.getLogger(DefaultGenericParamResolveServiceImpl.class);

    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        Map<String, Object> paramMap = new ConcurrentHashMap<>();
        try {
            paramMap = new ObjectMapper().readValue(body, Map.class);
            Class<?> c = Class.forName(parameterTypes);
            if (c.isArray()) {
                c = c.getComponentType();
            }
            boolean isPrimitiveOrWrapped = ClassUtils.isPrimitiveOrWrapper(c);
            if (isPrimitiveOrWrapped || c.equals(String.class)) {
                return new ImmutablePair<>(new String[] {parameterTypes}, paramMap.values().toArray(new Object[0]));
            }
        } catch (Exception e) {
            LOG.error("dubbo param build fail, ex:{}", e.getMessage());
        }
        return new ImmutablePair<>(new String[] {parameterTypes}, new Object[] {paramMap});
    }
}


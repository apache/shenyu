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

package org.dromara.soul.extend.demo.dubbo;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.web.plugin.dubbo.GenericParamResolveService;

/**
 * The type Custom generic param service.
 *
 * @author xiaoyu(Myth)
 */
public class CustomGenericParamServiceImpl implements GenericParamResolveService {


    /**
     * It parses itself based on the json string passed in by the body.
     * Return dubbo GenericService Invoker need param
     * see {@linkplain com.alibaba.dubbo.rpc.service.GenericService}
     *
     * @param body the body map
     * @param body the parameterTypes
     * @return the left is  parameter type.  the right is  Parameter Values.
     */
    @Override
    public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
        return new ImmutablePair<>(new String[]{body}, new Object[]{parameterTypes});
    }
}

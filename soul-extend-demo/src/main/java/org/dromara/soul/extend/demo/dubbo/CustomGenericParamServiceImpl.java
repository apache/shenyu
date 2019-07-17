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

import com.google.common.collect.Lists;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.DubboParamConstants;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.plugin.dubbo.GenericParamService;

import java.util.List;
import java.util.Map;

/**
 * The type Custom generic param service.
 *
 * @author xiaoyu(Myth)
 */
public class CustomGenericParamServiceImpl implements GenericParamService {

    /**
     * It parses itself based on the json string passed in by the body.
     * Return dubbo GenericService Invoker need param
     * see {@linkplain com.alibaba.dubbo.rpc.service.GenericService}
     * @param paramMap the param map
     * @return the left is  parameter type.  the right is  Parameter Values.
     */
    @Override
    public Pair<String[], Object[]> buildParameter(final Map<String, Object> paramMap) {
        List<String> paramType = Lists.newArrayList();
        List<Object> args = Lists.newArrayList();
        if (paramMap.containsKey(DubboParamConstants.PARAM_CLASS)) {
            List<String> clazz = GsonUtils.getInstance()
                    .fromList(paramMap.get(DubboParamConstants.PARAM_CLASS).toString(), String.class);
            paramType.addAll(clazz);
            final Object classParams = paramMap.get(DubboParamConstants.CLASS_PARAMS);
            args.addAll(GsonUtils.getInstance()
                    .fromJson(classParams.toString(), List.class));
        }
        return new ImmutablePair<>(paramType.toArray(new String[0]), args.toArray());
    }
}

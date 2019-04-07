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

package org.dromara.soul.web.plugin.dubbo;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.DubboParamConstants;
import org.dromara.soul.common.utils.GsonUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Generic param service.
 *
 * @author xiaoyu(Myth)
 */
public class GenericParamServiceImpl implements GenericParamService {

    @Override
    public Pair<String[], Object[]> buildParameter(Map<String, Object> paramMap) {
        List<String> paramList = Lists.newArrayList();
        List<Object> args = Lists.newArrayList();
        //如果参数里面包含class字段
        //如果参数里面包含class字段
        if (paramMap.containsKey(DubboParamConstants.PARAM_CLASS)) {
            List<String> clazz = GsonUtils.getInstance()
                    .fromList(paramMap.get(DubboParamConstants.PARAM_CLASS).toString(), String[].class);
            //设置参数为class 类型
            AtomicBoolean hasList = new AtomicBoolean(false);
            clazz.forEach(c -> {
                paramList.add(c);
                if (List.class.getName().equals(c)) {
                    hasList.set(true);
                }
            });

            if (hasList.get()) {
                final Object classParams = paramMap.get(DubboParamConstants.CLASS_PARAMS);
                List<Map> params = GsonUtils.getInstance().toListMap(classParams.toString());
                args.add(params);
            } else {
                final Object classParams = paramMap.get(DubboParamConstants.CLASS_PARAMS);
                args.addAll(GsonUtils.getInstance()
                        .fromJson(classParams.toString(), List.class));
            }
        }
        //如果Map参数里面包含 params字段  规定params 里面是json字符串转成Map key为类型，value为值
        if (paramMap.containsKey(DubboParamConstants.PARAMS)
                && !StringUtils.equals(String.valueOf(paramMap.get(DubboParamConstants.PARAMS)), "null")) {
            final Object params = paramMap.get(DubboParamConstants.PARAMS);
            final Map<String, Object> objectMap = GsonUtils.getInstance().toObjectMap(params.toString());
            objectMap.forEach((k, v) -> {
                //如果v是数组类型
                if (v instanceof JsonArray) {
                    List<String> arg = GsonUtils.getInstance().fromJson(v.toString(), List.class);
                    arg.forEach(j -> {
                        paramList.add(k);
                        args.add(j);
                    });
                } else {
                    paramList.add(k);
                    args.add(v);
                }
            });
        }
        return new ImmutablePair<>(paramList.toArray(new String[0]), args.toArray());
    }
}

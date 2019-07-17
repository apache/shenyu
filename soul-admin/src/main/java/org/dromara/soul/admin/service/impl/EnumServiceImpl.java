/*
 *   Licensed to the Apache Software Foundation (final ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (final the "License"); you may not use this file except in compliance with
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

package org.dromara.soul.admin.service.impl;

import com.google.common.collect.Maps;
import org.dromara.soul.admin.service.EnumService;
import org.dromara.soul.admin.vo.EnumVO;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.enums.MatchModeEnum;
import org.dromara.soul.common.enums.OperatorEnum;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RedisModeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.enums.SerializeEnum;
import org.dromara.soul.common.enums.WafEnum;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * EnumServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("enumService")
public class EnumServiceImpl implements EnumService {

    /**
     * find list of enum.
     *
     * @return {@linkplain Map}
     */
    @Override
    public Map<String, List<EnumVO>> list() {
        List<EnumVO> httpMethodEnums = Arrays.stream(HttpMethodEnum.values())
                .map(httpMethodEnum -> new EnumVO(null, httpMethodEnum.getName(), httpMethodEnum.getSupport()))
                .collect(Collectors.toList());

        List<EnumVO> loadBalanceEnums = Arrays.stream(LoadBalanceEnum.values())
                .map(loadBalanceEnum -> new EnumVO(loadBalanceEnum.getCode(), loadBalanceEnum.getName(), true))
                .collect(Collectors.toList());

        List<EnumVO> matchModeEnums = Arrays.stream(MatchModeEnum.values())
                .map(matchModeEnum -> new EnumVO(matchModeEnum.getCode(), matchModeEnum.getName(), true))
                .collect(Collectors.toList());

        List<EnumVO> operatorEnums =
                OperatorEnum.acquireSupport().stream().map(operatorEnum ->
                        new EnumVO(null, operatorEnum.getAlias(), operatorEnum.getSupport()))
                        .collect(Collectors.toList());

        List<EnumVO> paramTypeEnums = ParamTypeEnum.acquireSupport().stream()
                .map(paramTypeEnum -> new EnumVO(null, paramTypeEnum.getName(), paramTypeEnum.getSupport())).collect(Collectors.toList());

        List<EnumVO> pluginEnums = Arrays.stream(PluginEnum.values())
                .map(pluginEnum -> new EnumVO(pluginEnum.getCode(), pluginEnum.getName(), true))
                .collect(Collectors.toList());

        List<EnumVO> pluginTypeEnums = Arrays.stream(PluginTypeEnum.values())
                .map(pluginTypeEnum -> new EnumVO(null, pluginTypeEnum.getName(), true))
                .collect(Collectors.toList());

        List<EnumVO> rpcTypeEnums = RpcTypeEnum.acquireSupports().stream()
                .map(rpcTypeEnum -> new EnumVO(null, rpcTypeEnum.getName(), rpcTypeEnum.getSupport()))
                .collect(Collectors.toList());

        List<EnumVO> selectorTypeEnums = Arrays.stream(SelectorTypeEnum.values())
                .map(selectorTypeEnum -> new EnumVO(selectorTypeEnum.getCode(), selectorTypeEnum.getName(), true)).collect(Collectors.toList());

        List<EnumVO> serializeEnums = Arrays.stream(SerializeEnum.values())
                .map(serializeEnum -> new EnumVO(null, serializeEnum.getSerialize(), true))
                .collect(Collectors.toList());

        List<EnumVO> wafEnums = Arrays.stream(WafEnum.values())
                .map(wafEnum -> new EnumVO(wafEnum.getCode(), wafEnum.getName(), true))
                .collect(Collectors.toList());

        List<EnumVO> redisModeEnums = Arrays.stream(RedisModeEnum.values())
                .map(redisModeEnum -> new EnumVO(null, redisModeEnum.getName(), true))
                .collect(Collectors.toList());

        Map<String, List<EnumVO>> enums = Maps.newHashMap();
        enums.put("httpMethodEnums", httpMethodEnums);
        enums.put("loadBalanceEnums", loadBalanceEnums);
        enums.put("matchModeEnums", matchModeEnums);
        enums.put("operatorEnums", operatorEnums);
        enums.put("paramTypeEnums", paramTypeEnums);
        enums.put("pluginEnums", pluginEnums);
        enums.put("pluginTypeEnums", pluginTypeEnums);
        enums.put("rpcTypeEnums", rpcTypeEnums);
        enums.put("selectorTypeEnums", selectorTypeEnums);
        enums.put("serializeEnums", serializeEnums);
        enums.put("wafEnums", wafEnums);
        enums.put("redisModeEnums", redisModeEnums);
        return enums;
    }
}

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

package org.apache.shenyu.admin.service.register;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.PathUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

/**
 * spring cloud service register.
 */
@Service("springCloud")
public class ShenyuClientRegisterSpringCloudServiceImpl extends AbstractContextPathRegisterService {

    @Override
    public String rpcType() {
        return RpcTypeEnum.SPRING_CLOUD.getName();
    }

    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return GsonUtils.getInstance().toJson(SpringCloudSelectorHandle.builder().serviceId(metaDataDTO.getAppName()).build());
    }

    @Override
    protected String ruleHandler() {
        return new SpringCloudRuleHandle().toJson();
    }

    @Override
    protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {
        MetaDataService metaDataService = getMetaDataService();
        metaDataDTO.setPath(PathUtils.decoratorPath(metaDataDTO.getContextPath()));
        MetaDataDO metaDataDO = metaDataService.findByPath(metaDataDTO.getPath());
        metaDataService.saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }

    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        List<DivideUpstream> addList = buildDivideUpstreamList(uriList);
        SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getInstance().fromJson(selectorDO.getHandle(), SpringCloudSelectorHandle.class);
        List<DivideUpstream> existList = springCloudSelectorHandle.getDivideUpstreams();
        if (CollectionUtils.isEmpty(existList)) {
            return doHandler(springCloudSelectorHandle, addList, addList, selectorDO);
        }
        List<DivideUpstream> canAddList = new CopyOnWriteArrayList<>();
        List<DivideUpstream> diffList = addList.stream().filter(divideUpstream -> !existList.contains(divideUpstream)).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(diffList)) {
            canAddList.addAll(diffList);
            existList.addAll(diffList);
        }
        return doHandler(springCloudSelectorHandle, existList, canAddList, selectorDO);
    }

    private String doHandler(final SpringCloudSelectorHandle springCloudSelectorHandle,
                             final List<DivideUpstream> existList,
                             final List<DivideUpstream> canAddList,
                             final SelectorDO selectorDO) {
        springCloudSelectorHandle.setDivideUpstreams(existList);
        doSubmit(selectorDO.getId(), canAddList);
        return GsonUtils.getInstance().toJson(springCloudSelectorHandle);
    }

    private List<DivideUpstream> buildDivideUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream()
                .map(dto -> CommonUpstreamUtils.buildDefaultDivideUpstream(dto.getHost(), dto.getPort()))
                .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
    }

}

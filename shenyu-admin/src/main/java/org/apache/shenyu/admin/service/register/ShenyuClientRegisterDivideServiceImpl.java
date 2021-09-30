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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * spring mvc service register.
 */
@Service("http")
public class ShenyuClientRegisterDivideServiceImpl extends AbstractShenyuClientRegisterServiceImpl {
    
    @Override
    protected String pluginName() {
        return PluginEnum.DIVIDE.getName();
    }
    
    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return "";
    }
    
    @Override
    protected String ruleHandler() {
        return new DivideRuleHandle().toJson();
    }
    
    @Override
    protected void registerMetadata(final MetaDataRegisterDTO dto) {
        MetaDataDO exist = metaDataService.findByPath(dto.getPath());
        metaDataService.saveOrUpdateMetaData(exist, dto);
    }
    
    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        String handleAdd;
        List<DivideUpstream> addList = buildDivideUpstreamList(uriList);
        List<DivideUpstream> canAddList = new ArrayList<>();
        if (StringUtils.isBlank(selectorDO.getHandle())) {
            handleAdd = GsonUtils.getInstance().toJson(addList);
            canAddList = addList;
        } else {
            List<DivideUpstream> existList = GsonUtils.getInstance().fromList(selectorDO.getHandle(), DivideUpstream.class);
            for (DivideUpstream exist : existList) {
                for (DivideUpstream add : addList ) {
                    if (!exist.getUpstreamUrl().equals(add.getUpstreamUrl())) {
                        existList.add(add);
                        canAddList.add(add);
                    }
                }
            }
            handleAdd = GsonUtils.getInstance().toJson(existList);
        }
        doSubmit(selectorDO.getId(), canAddList);
        return handleAdd;
    }
    
    private List<DivideUpstream> buildDivideUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream().map(dto -> CommonUpstreamUtils.buildDefaultDivideUpstream(dto.getHost(), dto.getPort())).collect(Collectors.toList());
    }
}

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
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * tars service register.
 */
@Service("tars")
public class ShenyuClientRegisterTarsServiceImpl extends AbstractShenyuClientRegisterServiceImpl {
    
    @Override
    protected String pluginName() {
        return PluginEnum.TARS.getName();
    }
    
    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return "";
    }
    
    @Override
    protected String ruleHandler() {
        return "";
    }
    
    @Override
    protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {
        MetaDataDO exist = metaDataService.findByServiceNameAndMethodName(metaDataDTO.getServiceName(), metaDataDTO.getMethodName());
        metaDataService.saveOrUpdateMetaData(exist, metaDataDTO);
    }
    
    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        String handleAdd;
        List<TarsUpstream> addList = buildTarsUpstreamList(uriList);
        List<TarsUpstream> canAddList = new ArrayList<>();
        if (StringUtils.isBlank(selectorDO.getHandle())) {
            handleAdd = GsonUtils.getInstance().toJson(addList);
            canAddList = addList;
        } else {
            List<TarsUpstream> existList = GsonUtils.getInstance().fromList(selectorDO.getHandle(), TarsUpstream.class);
            for (TarsUpstream exist : existList) {
                for (TarsUpstream add : addList ) {
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
    
    private List<TarsUpstream> buildTarsUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream().map(dto -> CommonUpstreamUtils.buildDefaultTarsUpstream(dto.getHost(), dto.getPort())).collect(Collectors.toList());
    }
}

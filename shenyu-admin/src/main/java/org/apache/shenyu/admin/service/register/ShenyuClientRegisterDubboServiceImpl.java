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
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * dubbo service register.
 */
@Service("dubbo")
public class ShenyuClientRegisterDubboServiceImpl extends AbstractShenyuClientRegisterServiceImpl {
    
    /**
     * Plugin name string.
     *
     * @return the string
     */
    @Override
    protected String pluginName() {
        return PluginEnum.DUBBO.getName();
    }
    
    /**
     * Selector handler string.
     *
     * @return the string
     */
    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return "";
    }
    
    /**
     * Rule handler string.
     *
     * @return the string
     */
    @Override
    protected String ruleHandler() {
        return new DubboRuleHandle().toJson();
    }
    
    /**
     * Register metadata.
     *
     * @param metaDataDTO the meta data dto
     */
    @Override
    protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {
        MetaDataDO exist = metaDataService.findByPath(metaDataDTO.getPath());
        metaDataService.saveOrUpdateMetaData(exist, metaDataDTO);
    }
    
    /**
     * Build handle string.
     *
     * @param uriList the uri list
     * @param selectorDO the selector do
     * @return the string
     */
    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        String handleAdd;
        List<DubboUpstream> addList = buildDubboUpstreamList(uriList);
        List<DubboUpstream> canAddList = new ArrayList<>();
        if (StringUtils.isBlank(selectorDO.getHandle())) {
            handleAdd = GsonUtils.getInstance().toJson(addList);
            canAddList = addList;
        } else {
            List<DubboUpstream> existList = GsonUtils.getInstance().fromList(selectorDO.getHandle(), DubboUpstream.class);
            for (DubboUpstream exist : existList) {
                for (DubboUpstream add : addList ) {
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
    
    private List<DubboUpstream> buildDubboUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream().map(dto -> CommonUpstreamUtils.buildDefaultDubboUpstream(dto.getHost(), dto.getPort())).collect(Collectors.toList());
    }
}

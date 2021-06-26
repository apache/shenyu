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

import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

/**
 * dubbo service register.
 */
@Service("dubbo")
public class ShenyuClientRegisterDubboServiceImpl extends AbstractShenyuClientRegisterServiceImpl {

    private final MetaDataService metaDataService;

    private final SelectorService selectorService;

    private final PluginService pluginService;

    private final RuleService ruleService;

    public ShenyuClientRegisterDubboServiceImpl(final MetaDataService metaDataService,
                                                final SelectorService selectorService,
                                                final RuleService ruleService,
                                                final PluginService pluginService) {
        this.metaDataService = metaDataService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.pluginService = pluginService;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public synchronized String register(final MetaDataRegisterDTO dto) {
        MetaDataDO exist = metaDataService.findByPath(dto.getPath());
        saveOrUpdateMetaData(exist, dto);
        String selectorId = handlerSelector(dto);
        handlerRule(selectorId, dto, exist);
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    public void saveOrUpdateMetaData(final MetaDataDO exist, final MetaDataRegisterDTO metaDataDTO) {
        metaDataService.saveOrUpdateMetaData(exist, metaDataDTO);
    }

    @Override
    public String handlerSelector(final MetaDataRegisterDTO metaDataDTO) {
        SelectorDO selectorDO = selectorService.findByName(metaDataDTO.getContextPath());
        if (Objects.nonNull(selectorDO)) {
            return selectorDO.getId();
        }
        return selectorService.register(registerSelector(metaDataDTO.getContextPath(), pluginService.selectIdByName(metaDataDTO.getRpcType())));
    }

    @Override
    public void handlerRule(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final MetaDataDO exist) {
        ruleService.register(registerRule(selectorId, metaDataDTO.getPath(), PluginEnum.DUBBO.getName(), metaDataDTO.getRuleName()),
                metaDataDTO.getPath(), false);
    }
}

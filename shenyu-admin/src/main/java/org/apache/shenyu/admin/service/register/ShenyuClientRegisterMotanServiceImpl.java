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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * motan service register.
 */
@Service("motan")
public class ShenyuClientRegisterMotanServiceImpl extends AbstractShenyuClientRegisterServiceImpl {
    
    /**
     * Plugin name string.
     *
     * @return the string
     */
    @Override
    protected String pluginName() {
        return PluginEnum.MOTAN.getName();
    }
    
    /**
     * Selector handler string.
     *
     * @param metaDataDTO
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
        return "";
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
        return "";
    }
}

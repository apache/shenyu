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

package org.apache.shenyu.admin.service.converter;

import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * The type Spring cloud selector handle converter.
 */
@Component
public class SpringCloudSelectorHandleConverter extends AbstractSelectorHandleConverter implements SelectorHandleConverter {
    
    @Override
    public List<CommonUpstream> convertUpstream(final String handle) {
        return CommonUpstreamUtils.convertCommonUpstreamList(convert(handle).getDivideUpstreams());
    }
    
    @Override
    public String pluginName() {
        return PluginEnum.SPRING_CLOUD.getName();
    }
    
    @Override
    protected Object doHandle(final String handle, final List<CommonUpstream> aliveList) {
        SpringCloudSelectorHandle selectorHandle = convert(handle);
        List<DivideUpstream> existList = updateStatusAndFilter(selectorHandle.getDivideUpstreams(), aliveList);
        aliveList.stream().filter(alive -> !existList.stream().anyMatch(valid -> valid.getUpstreamUrl().equals(alive.getUpstreamUrl())))
                .forEach(alive -> existList.add(CommonUpstreamUtils.buildDefaultAliveDivideUpstream(alive.getUpstreamUrl())));
        existList.removeIf(e -> aliveList.stream().noneMatch(alive -> alive.getUpstreamUrl().equals(e.getUpstreamUrl())));
        selectorHandle.setDivideUpstreams(existList);
        return selectorHandle;
    }
    
    private SpringCloudSelectorHandle convert(final String handle) {
        return GsonUtils.getInstance().fromJson(handle, SpringCloudSelectorHandle.class);
    }
}

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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The type Abstract selector handle converter.
 */
public abstract class AbstractSelectorHandleConverter implements SelectorHandleConverter {
    public static final String EMPTY_LIST_JSON = "[]";

    /**
     * Do handle object.
     *
     * @param handle the handle
     * @param aliveList the alive list
     * @return the object
     */
    protected abstract Object doHandle(String handle, List<CommonUpstream> aliveList);
    
    /**
     * Handler string.
     *
     * @param handle the handle
     * @param aliveList the upstream live list
     * @return the string
     */
    @Override
    public String handler(final String handle, final List<CommonUpstream> aliveList) {
        if ((StringUtils.isEmpty(handle) || EMPTY_LIST_JSON.equals(handle)) && CollectionUtils.isEmpty(aliveList)) {
            return EMPTY_LIST_JSON;
        }
        return GsonUtils.getInstance().toJson(doHandle(StringUtils.isEmpty(handle) ? EMPTY_LIST_JSON : handle, aliveList));
    }

    /**
     * Update upstream status and remove invalid.
     *
     * @param existList the existList
     * @param aliveList the aliveList
     * @return the valid existList
     */
    @Override
    public <T extends CommonUpstream> List<T> updateStatusAndFilter(final List<T> existList, final List<? extends CommonUpstream> aliveList) {
        if (aliveList == null) {
            return Collections.emptyList();
        }
        long currentTimeMillis = System.currentTimeMillis();
        List<T> validExistList = existList.stream()
                .filter(e -> e.isStatus() || e.getTimestamp() > currentTimeMillis - TimeUnit.SECONDS.toMillis(UpstreamCheckService.getZombieRemovalTimes())
                        || aliveList.stream().anyMatch(alive -> alive.getUpstreamUrl().equals(e.getUpstreamUrl())))
                .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
        validExistList.stream()
                .filter(upstream -> !upstream.isStatus() && aliveList.stream().anyMatch(alive -> alive.getUpstreamUrl().equals(upstream.getUpstreamUrl())))
                .forEach(upstream -> {
                    upstream.setStatus(true);
                    upstream.setTimestamp(currentTimeMillis);
                });
        validExistList.stream()
                .filter(upstream -> !aliveList.stream().anyMatch(alive -> alive.getUpstreamUrl().equals(upstream.getUpstreamUrl())))
                .forEach(upstream -> {
                    upstream.setStatus(false);
                    upstream.setTimestamp(currentTimeMillis);
                });
        return validExistList;
    }
}

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

package org.apache.shenyu.admin.controller;

import com.google.common.collect.Maps;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;


/**
 * This Controller only when HttpLongPollingDataChangedListener exist, will take effect.
 */
@RestController
@ResponseBody
@RequestMapping("/configs")
@ConditionalOnProperty(prefix = "shenyu.sync.http", name = "enabled", havingValue = "true")
public class ConfigController {
    
    private final HttpLongPollingDataChangedListener httpLongPollingDataChangedListener;
    
    private final NamespaceService namespaceService;
    
    public ConfigController(final HttpLongPollingDataChangedListener httpLongPollingDataChangedListener,
                            final NamespaceService namespaceService) {
        this.httpLongPollingDataChangedListener = httpLongPollingDataChangedListener;
        this.namespaceService = namespaceService;
    }
    
    /**
     * Fetch configs shenyu result.
     *
     * @param groupKeys   the group keys
     * @param namespaceId namespaceId
     * @return the shenyu result
     */
    @GetMapping("/fetch")
    public ShenyuAdminResult fetchConfigs(@NotNull final String[] groupKeys, final String namespaceId) {
        if (StringUtils.isEmpty(namespaceId)) {
            throw new ShenyuAdminException("namespaceId is null");
        }
        NamespaceVO existNamespace = namespaceService.findByNamespaceId(namespaceId);
        if (StringUtils.isNotEmpty(namespaceId) && ObjectUtils.isEmpty(existNamespace)) {
            throw new ShenyuAdminException("namespace is not exist");
        }
        Map<String, ConfigData<?>> result = Maps.newHashMap();
        for (String groupKey : groupKeys) {
            ConfigData<?> data = httpLongPollingDataChangedListener.fetchConfig(ConfigGroupEnum.valueOf(groupKey), namespaceId);
            result.put(groupKey, data);
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, result);
    }
    
    /**
     * Listener.
     *
     * @param request  the request
     * @param response the response
     */
    @PostMapping(value = "/listener")
    public void listener(final HttpServletRequest request, final HttpServletResponse response) {
        httpLongPollingDataChangedListener.doLongPolling(request, response);
    }
    
}

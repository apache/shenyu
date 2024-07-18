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
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.util.Map;

import static org.apache.shenyu.common.constant.AdminConstants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;

/**
 * This Controller only when HttpLongPollingDataChangedListener exist, will take effect.
 */
@ResponseBody
@RequestMapping("/configs")
public class ConfigController {

    private final HttpLongPollingDataChangedListener longPollingListener;

    private final NamespaceService namespaceService;

    public ConfigController(final HttpLongPollingDataChangedListener longPollingListener, NamespaceService namespaceService) {
        this.longPollingListener = longPollingListener;
        this.namespaceService = namespaceService;
    }

    /**
     * Fetch configs shenyu result.
     *
     * @param groupKeys the group keys
     * @return the shenyu result
     */
    @GetMapping("/fetch")
    public ShenyuAdminResult fetchConfigs(@NotNull final String[] groupKeys, final String namespaceIdParams) {
        String namespaceId = namespaceIdParams;
        if (StringUtils.isNotEmpty(namespaceId) && ObjectUtils.isEmpty(namespaceService.findById(namespaceId))) {
            throw new ShenyuAdminException("namespaceId is not exist");
        }
        if (StringUtils.isEmpty(namespaceId)) {
            namespaceId = SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;
        }
        Map<String, ConfigData<?>> result = Maps.newHashMap();
        for (String groupKey : groupKeys) {
            ConfigData<?> data = longPollingListener.fetchConfig(ConfigGroupEnum.valueOf(groupKey), namespaceId);
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
        longPollingListener.doLongPolling(request, response);
    }

}

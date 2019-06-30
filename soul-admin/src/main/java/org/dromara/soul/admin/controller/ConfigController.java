/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.admin.controller;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.admin.listener.http.HttpLongPollingDataChangedListener;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * This Controller only when HttpLongPollingDataChangedListener exist, will take effect.
 * @author huangxiaofeng
 * @date 2019/6/24 23:00
 */
@ConditionalOnBean(HttpLongPollingDataChangedListener.class)
@RestController
@RequestMapping("/configs")
@Slf4j
public class ConfigController {

    @Resource
    private HttpLongPollingDataChangedListener longPollingListener;

    @GetMapping(path = "", params = "groupKeys")
    public SoulResult fetchConfigs(@NotNull String[] groupKeys) {
        try {
            Map<String, ConfigData> result = new HashMap<>();
            for (String groupKey : groupKeys) {
                ConfigData data = longPollingListener.fetchConfig( ConfigGroupEnum.valueOf(groupKey) );
                result.put( groupKey, data );
            }
            return SoulResult.success("success", result);
        } catch (Exception e) {
            log.error("fetch all configs error.", e);
            return SoulResult.error("fetch all configs error: " + e.getMessage());
        }
    }

    /**
     * Listen for changes to group data.
     */
    @PostMapping(value = "/listener")
    public void listener(HttpServletRequest request, HttpServletResponse response) {
        longPollingListener.doLongPolling(request, response);
    }

}

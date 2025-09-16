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

package org.apache.shenyu.admin.listener;

import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.admin.utils.ShenyuDomain;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.IpUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;


/**
 * ApplicationStartListener.
 */
@Component
public class ApplicationStartListener implements ApplicationListener<WebServerInitializedEvent> {
    
    @Value("${server.servlet.context-path:}")
    private String contextPath;
    
    @Resource
    private ShenyuRunningModeService shenyuRunningModeService;
    
    @Override
    public void onApplicationEvent(final WebServerInitializedEvent event) {
        int port = event.getWebServer().getPort();
        final String host = IpUtils.getHost();
        String domain = System.getProperty(Constants.HTTP_PATH);
        if (StringUtils.isBlank(domain)) {
            domain = System.getenv(Constants.HTTP_PATH);
        }
        if (StringUtils.isBlank(domain)) {
            ShenyuDomain.getInstance().setHttpPath("http://" + String.join(":", host, String.valueOf(port)) + contextPath);
        } else {
            ShenyuDomain.getInstance().setHttpPath(domain);
        }
        
        shenyuRunningModeService.start(host, port, contextPath);
    }
}

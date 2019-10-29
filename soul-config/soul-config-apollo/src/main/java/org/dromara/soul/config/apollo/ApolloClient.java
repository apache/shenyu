/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.config.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import com.ctrip.framework.apollo.ConfigService;
import com.ctrip.framework.apollo.core.ConfigConsts;
import com.ctrip.framework.apollo.model.ConfigChange;
import com.ctrip.framework.apollo.model.ConfigChangeEvent;
import org.dromara.soul.common.utils.StringUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * The type Apollo client.
 *
 * @author xiaoyu
 */
class ApolloClient {

    /**
     * Pull.
     */
    static Map<String, Object> pull(ApolloConfig apolloConfig) {
        Map<String, Object> resultMap = new HashMap<>();
        System.setProperty(ConfigConsts.APOLLO_META_KEY, apolloConfig.getMetaServer());
        System.setProperty("app.id", apolloConfig.getAppId());
        System.setProperty("env", "dev");
        System.setProperty("apollo.configService", "http://121.43.171.129:8080");
        if (StringUtils.isNotBlank(apolloConfig.getApplication())) {
            System.setProperty(ConfigConsts.NAMESPACE_APPLICATION, apolloConfig.getApplication());
        }
        Config config = ConfigService.getAppConfig();
        Set<String> propertyNames = config.getPropertyNames();
        for (String propertyName : propertyNames) {
            String value = config.getProperty(propertyName, "");
            resultMap.put(propertyName, value);
        }
        return resultMap;
    }
}

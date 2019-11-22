/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.dubbo;

import com.alibaba.dubbo.config.ApplicationConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.dromara.soul.common.http.URL;

/**
 * RegistryReference
 *
 * @author sixh
 */
class RegistryReference {
    /**
     * Application configuration.
     */
    private ApplicationConfig applicationConfig;

    private URL url;

    /**
     * Registration information processing.
     */
    private RegistryConfig registryConfig;

    private static final Map<URL, RegistryReference> REFERENCE_MAP = new ConcurrentHashMap<>();

    private static RegistryReference init(URL url) {
        RegistryReference registryReference = REFERENCE_MAP.get(url);
        if (registryReference == null) {
            synchronized (REFERENCE_MAP) {
                registryReference = REFERENCE_MAP.get(url);
                if (registryReference == null) {
                    RegistryReference rr = new RegistryReference();
                    ApplicationConfig applicationConfig = new ApplicationConfig("soul_application");
                    applicationConfig.setQosEnable(false);
                    RegistryConfig registryConfig = new RegistryConfig(url.fullString());
                    registryConfig.setProtocol("dubbo");
                    registryConfig.setId("soul_application");
                    registryConfig.setRegister(false);
                    rr.applicationConfig = applicationConfig;
                    rr.registryConfig = registryConfig;
                    rr.url = url;
                    REFERENCE_MAP.put(url, rr);
                    registryReference = REFERENCE_MAP.get(url);
                }
            }
        }
        return registryReference;
    }

    /**
     * Get registry reference.
     *
     * @param url the url
     * @return the registry reference.
     */
    static RegistryReference get(URL url) {
        return init(url);
    }

    /**
     * Gets application config.
     *
     * @return the application config.
     */
    ApplicationConfig getApplicationConfig() {
        return applicationConfig;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public URL getUrl() {
        return url;
    }

    /**
     * Gets registry config.
     *
     * @return the registry config
     */
    RegistryConfig getRegistryConfig() {
        return registryConfig;
    }
}

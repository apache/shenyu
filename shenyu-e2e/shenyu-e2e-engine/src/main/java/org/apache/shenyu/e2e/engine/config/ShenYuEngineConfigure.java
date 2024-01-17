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

package org.apache.shenyu.e2e.engine.config;

import org.apache.shenyu.e2e.config.ServiceConfigure;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest.Environment;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

public class ShenYuEngineConfigure {
    
    private Map<String, ServiceConfigure> serviceConfigureMap;
    
    /**
     * get serviceConfigureMap.
     *
     * @return serviceConfigureMap
     */
    public Map<String, ServiceConfigure> getServiceConfigureMap() {
        return serviceConfigureMap;
    }
    
    /**
     * set serviceConfigureMap.
     *
     * @param serviceConfigureMap serviceConfigureMap
     */
    public void setServiceConfigureMap(final Map<String, ServiceConfigure> serviceConfigureMap) {
        this.serviceConfigureMap = serviceConfigureMap;
    }
    
    /**
     * from annotation.
     *
     * @param annotation annotation
     * @return ShenYuE2EEngineConfigure
     */
    public static ShenYuEngineConfigure fromAnnotation(final ShenYuTest annotation) {
        ShenYuTest.Environment[] environments = annotation.environments();
        if (environments.length == 0) {
            throw new IllegalArgumentException("ShenYuE2ETest.Environment is empty");
        }
        Map<String, List<Environment>> environmentMap = Arrays.stream(environments)
                .collect(Collectors.groupingBy(ShenYuTest.Environment::serviceName));
        Map<String, ServiceConfigure> map = new HashMap<>();
        environmentMap.forEach((k, v) -> {
            Environment environment = environmentMap.get(k).get(0);
            ServiceConfigure serviceConfigure = new ServiceConfigure();
            serviceConfigure.setServiceName(environment.serviceName());
            serviceConfigure.setModuleName(environment.service().moduleName());
            serviceConfigure.setBaseUrl(environment.service().baseUrl());
            serviceConfigure.setPort(environment.service().port());
            serviceConfigure.setSchema(environment.service().schema());
            serviceConfigure.setServiceType(environment.service().type());
            serviceConfigure.setParameters(toProperties(environment.service().parameters()));
            map.put(k, serviceConfigure);
        });
        
        ShenYuEngineConfigure engineConfigure = new ShenYuEngineConfigure();
        engineConfigure.setServiceConfigureMap(map);
        return engineConfigure;
    }
    
    private static Properties toProperties(final ShenYuTest.Parameter[] parameters) {
        Properties properties = new Properties();
        Arrays.stream(parameters).forEach(p -> properties.put(p.key(), p.value()));
        return properties;
    }
    
}

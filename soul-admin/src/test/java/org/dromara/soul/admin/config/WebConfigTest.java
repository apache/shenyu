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

package org.dromara.soul.admin.config;

import com.alibaba.fastjson.JSONObject;
import org.dromara.soul.admin.AbstractConfigurationTest;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test cases for WebConfig.
 *
 * @author Yejiajun
 */
public final class WebConfigTest extends AbstractConfigurationTest {

    @Test
    public void testaddCorsMappings() {
        CorsRegistryExtend registry = new CorsRegistryExtend();
        WebConfig webConfig = new WebConfig();
        webConfig.addCorsMappings(registry);
        String registryString = JSONObject.toJSONString(registry.getCorsConfigurations());
        Assert.assertEquals(corsRegistryJSONStringExtendBuild(), registryString);
    }

    private String corsRegistryJSONStringExtendBuild() {
        CorsRegistryExtend registry = new CorsRegistryExtend();
        registry.addMapping("/**")
                .allowedHeaders("Access-Control-Allow-Origin",
                        "*",
                        "Access-Control-Allow-Methods",
                        "POST, GET, OPTIONS, PUT, DELETE",
                        "Access-Control-Allow-Headers",
                        "Origin, X-Requested-With, Content-Type, Accept")
                .allowedOrigins("*")
                .allowedMethods("*");
        return JSONObject.toJSONString(registry.getCorsConfigurations());
    }
}

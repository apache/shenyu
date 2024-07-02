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

package org.apache.shenyu.client.springmvc.proceeor.register;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.matcher.BaseAnnotationApiProcessor;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;

import java.util.Objects;

/**
 * RequestMappingProcessorImpl.<br>
 * About support for {@link ShenyuSpringMvcClient} annotations
 *
 * @see ShenyuSpringMvcClient
 */
public class ShenyuSpringMvcClientProcessorImpl extends BaseAnnotationApiProcessor<ShenyuSpringMvcClient> {
    
    @Override
    public void process(final ApiBean apiBean, final ShenyuSpringMvcClient annotation) {
        apiBean.setBeanPath(annotation.path()[0]);
        apiBean.addProperties("desc", annotation.desc());
        if (StringUtils.isNotBlank(apiBean.getPropertiesValue("rule"))) {
            apiBean.addProperties("rule", annotation.ruleName());
        }
        apiBean.addProperties("value", annotation.value()[0]);
        apiBean.addProperties("enabled", Objects.toString(annotation.enabled()));
        apiBean.addProperties("registerMetaData", Objects.toString(annotation.registerMetaData()));
        if (!annotation.registerMetaData()) {
            apiBean.setStatus(ApiBean.Status.CAN_NO_BE_REGISTERED);
        } else {
            apiBean.setStatus(ApiBean.Status.REGISTRABLE_API);
        }
        // This annotation is on the support class, and all APIs will be registered
        for (ApiBean.ApiDefinition definition : apiBean.getApiDefinitions()) {
            definition.setStatus(apiBean.getStatus());
        }
    }
    
    @Override
    public void process(final ApiBean.ApiDefinition definition, final ShenyuSpringMvcClient annotation) {
        definition.setMethodPath(annotation.path()[0]);
        definition.addProperties("desc", annotation.desc());
        definition.addProperties("rule", annotation.ruleName());
        definition.addProperties("value", annotation.value()[0]);
        definition.addProperties("enabled", Objects.toString(annotation.enabled()));
        definition.addProperties("registerMetaData", Objects.toString(annotation.registerMetaData()));
        if (!annotation.registerMetaData()) {
            definition.setStatus(ApiBean.Status.CAN_NO_BE_REGISTERED);
        } else {
            definition.setStatus(ApiBean.Status.REGISTRABLE);
        }
    }
    
    @Override
    public Class<ShenyuSpringMvcClient> matchAnnotation() {
        return ShenyuSpringMvcClient.class;
    }
}

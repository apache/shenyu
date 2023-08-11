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

package org.apache.shenyu.client.springmvc.proceeor.extractor;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.matcher.ApiAnnotationProcessor;
import org.apache.shenyu.client.core.register.matcher.ExtractorProcessor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;
import java.util.Objects;

/**
 * RequestMappingProcessor.
 */
public class RequestMappingProcessor implements ApiAnnotationProcessor<RequestMapping>, ExtractorProcessor {
    
    @Override
    public List<String> supportedClient() {
        return ListUtil.of(RpcTypeEnum.HTTP.getName());
    }
    
    @Override
    public void process(final ApiBean api, final RequestMapping annotation) {
        
        String beanPath = Objects.isNull(annotation) ? "" : getPath(annotation);
        // rewrite api path
        api.setBeanPath(beanPath);
        
        if (Objects.nonNull(annotation)) {
            api.addProperties("consumes", String.join(",", annotation.consumes()));
            api.addProperties("produces", String.join(",", annotation.produces()));
        }
        
        // Get additional values from the annotation.
        // TO_DO : Provides support annotation extensions
    }
    
    @Override
    public void process(final ApiBean.ApiDefinition definition, final RequestMapping annotation) {
        // rewrite api path
        definition.setMethodPath(getPath(annotation));
        
        definition.addProperties("consumes", String.join(",", annotation.consumes()));
        definition.addProperties("produces", String.join(",", annotation.produces()));
        
        definition.addProperties("desc", definition.getApiMethodName());
        definition.addProperties("rule", "");
        definition.addProperties("value", getPath(annotation));
        // Get additional values from the annotation.
        // TO_DO : Provides support annotation extensions
    }
    
    @Override
    public Class<RequestMapping> matchAnnotation() {
        return RequestMapping.class;
    }
    
    private String getPath(@NonNull final RequestMapping requestMapping) {
        if (ArrayUtils.isEmpty(requestMapping.path())) {
            return "";
        }
        return requestMapping.path()[0];
    }
}

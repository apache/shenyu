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

package org.apache.shenyu.client.springmvc.register;

import org.apache.shenyu.client.core.register.extractor.BaseAnnotationApiBeansExtractor;
import org.apache.shenyu.client.core.register.extractor.RpcApiBeansExtractor;
import org.apache.shenyu.client.springmvc.proceeor.extractor.RequestMappingProcessor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * SpringMvcApiBeansExtractor.<br>
 * It is used to support the extraction of the springMVC interface and convert it to the API Bean protocol.
 */
public class SpringMvcApiBeansExtractor extends BaseAnnotationApiBeansExtractor implements RpcApiBeansExtractor {
    
    @Override
    public String clientName() {
        return RpcTypeEnum.HTTP.getName();
    }
    
    /**
     * default.
     *
     * @return default
     */
    public static SpringMvcApiBeansExtractor buildDefaultSpringMvcApiBeansExtractor() {
        final SpringMvcApiBeansExtractor extractor = new SpringMvcApiBeansExtractor();
        
        // Annotations supported by class
        extractor.addSupportedApiAnnotations(Controller.class);
        extractor.addSupportedApiAnnotations(RequestMapping.class);
        
        // Annotations supported by the method
        extractor.addSupportedApiDefinitionAnnotations(RequestMapping.class);
        
        extractor.addExtractorProcessor(new RequestMappingProcessor());
        return extractor;
    }
}

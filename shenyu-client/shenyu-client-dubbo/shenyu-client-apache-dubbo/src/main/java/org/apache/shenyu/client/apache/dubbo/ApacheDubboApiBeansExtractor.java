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

package org.apache.shenyu.client.apache.dubbo;

import org.apache.dubbo.config.annotation.DubboService;
import org.apache.dubbo.config.annotation.Service;
import org.apache.shenyu.client.apache.dubbo.processor.extractor.DubboServiceProcessor;
import org.apache.shenyu.client.apache.dubbo.processor.extractor.ServiceProcessor;
import org.apache.shenyu.client.core.register.extractor.BaseAnnotationApiBeansExtractor;
import org.apache.shenyu.client.core.register.extractor.RpcApiBeansExtractor;
import org.apache.shenyu.common.enums.RpcTypeEnum;

public class ApacheDubboApiBeansExtractor extends BaseAnnotationApiBeansExtractor implements RpcApiBeansExtractor {
    
    public ApacheDubboApiBeansExtractor() {
        // Annotations supported by class
        addSupportedApiAnnotations(DubboService.class);
        addSupportedApiAnnotations(Service.class);
        
        // Annotations supported by the method
        
        addExtractorProcessor(new DubboServiceProcessor());
        addExtractorProcessor(new ServiceProcessor());
    }
    
    @Override
    public String clientName() {
        return RpcTypeEnum.DUBBO.getName();
    }
    
}

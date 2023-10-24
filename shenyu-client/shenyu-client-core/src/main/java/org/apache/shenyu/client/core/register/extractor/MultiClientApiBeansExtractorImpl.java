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

package org.apache.shenyu.client.core.register.extractor;

import org.apache.shenyu.client.core.register.ApiBean;
import org.springframework.context.ApplicationContext;

import java.util.List;
import java.util.stream.Collectors;

/**
 * MultiClientApiBeansExtractorImpl.
 * Multi-client collector
 */
public class MultiClientApiBeansExtractorImpl implements ApiBeansExtractor {
    
    private final List<RpcApiBeansExtractor> rpcApiBeansExtractors;
    
    public MultiClientApiBeansExtractorImpl(final List<RpcApiBeansExtractor> rpcApiBeansExtractors) {
        this.rpcApiBeansExtractors = rpcApiBeansExtractors;
    }
    
    @Override
    public List<ApiBean> extract(final ApplicationContext applicationContext) {
        return rpcApiBeansExtractors.stream()
                .flatMap(e -> e.extract(applicationContext).stream())
                .collect(Collectors.toList());
    }
}

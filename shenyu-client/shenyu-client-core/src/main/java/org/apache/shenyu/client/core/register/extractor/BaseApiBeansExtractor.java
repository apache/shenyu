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
import org.apache.shenyu.client.core.register.matcher.ExtractorProcessor;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * BaseApiBeansExtractor.<br>
 * Used to demonstrate the basic behavior of the extractor.
 */
public abstract class BaseApiBeansExtractor implements RpcApiBeansExtractor {
    
    protected static final Logger LOG = LoggerFactory.getLogger(BaseApiBeansExtractor.class);
    
    private final List<ExtractorProcessor> extractorProcessors = new ArrayList<>(5);
    
    @Override
    public List<ApiBean> extract(final ApplicationContext applicationContext) {
        
        Map<String, Object> supportBeans = extractSupportBeans(applicationContext);
        if (Objects.isNull(supportBeans)) {
            return Collections.emptyList();
        }
        return extract(applicationContext, supportBeans);
    }
    
    /**
     * extract.
     *
     * @param applicationContext applicationContext
     * @param supportBeans       supportBeans
     * @return api
     */
    @NotNull
    public List<ApiBean> extract(final ApplicationContext applicationContext, final Map<String, Object> supportBeans) {
        return supportBeans.entrySet()
                .stream()
                .map(entity -> beanToApi(entity.getKey(), entity.getValue(), applicationContext))
                .filter(this::apiPostFilter)
                .peek(this::apiPostProcess)
                .collect(Collectors.toList());
    }
    
    /**
     * Post-processing, you can filter the information that does not need to be registered again through the secondary implementation.<br>
     * default is pass.
     *
     * @param api api
     * @return true is pass
     */
    protected boolean apiPostFilter(final ApiBean api) {
        return Objects.nonNull(api);
    }
    
    /**
     * post process api.
     *
     * @param api api
     */
    protected void apiPostProcess(final ApiBean api) {
        extractorProcessors.forEach(apiAnnotationProcessor -> apiAnnotationProcessor.process(api));
        LOG.debug("[Shenyu Client] extract api info [{}]", api);
    }
    
    /**
     * Convert bean instances to APIs.
     *
     * @param key                is bean name
     * @param value              is bean instance
     * @param applicationContext applicationContext
     * @return API
     */
    protected ApiBean beanToApi(final String key, final Object value, final ApplicationContext applicationContext) {
        final List<ApiBean.ApiDefinition> apiDefinitions = extractSupportDefinitions(value, applicationContext);
        if (CollectionUtils.isEmpty(apiDefinitions)) {
            // not has definitions [skip]
            return null;
        }
        return newApi(key, value, apiDefinitions);
    }
    
    /**
     * new api.
     *
     * @param key            bean name
     * @param value          bean instance
     * @param apiDefinitions apiDefinitions.
     * @return api
     */
    protected ApiBean newApi(final String key, final Object value, final List<ApiBean.ApiDefinition> apiDefinitions) {
        return new ApiBean(clientName(), key, value, apiDefinitions);
    }
    
    /**
     * Extracts the collection of instances of supported definitions.<br>
     *
     * @param bean               bean instance.
     * @param applicationContext applicationContext.
     * @return bean map
     */
    protected List<ApiBean.ApiDefinition> extractSupportDefinitions(final Object bean, final ApplicationContext applicationContext) {
        return extractSupportMethods(bean, applicationContext)
                .stream()
                .map(method -> newApiDefinition(method, applicationContext))
                .filter(this::definitionPostFilter)
                .peek(this::definitionPostProcess)
                .collect(Collectors.toList());
        
    }
    
    /**
     * definitionPostProcess.
     *
     * @param apiDefinition apiDefinition
     */
    protected void definitionPostProcess(final ApiBean.ApiDefinition apiDefinition) {
        extractorProcessors.forEach(apiAnnotationProcessor -> apiAnnotationProcessor.process(apiDefinition));
        LOG.debug("[Shenyu Client] extract api definition info [{}]", apiDefinition);
    }
    
    /**
     * definitionPostFilter.
     *
     * @param apiDefinition apiDefinition
     * @return true
     */
    protected boolean definitionPostFilter(final ApiBean.ApiDefinition apiDefinition) {
        return Objects.nonNull(apiDefinition);
    }
    
    /**
     * new ApiDefinition.
     *
     * @param method             method
     * @param applicationContext applicationContext
     * @return definition
     */
    protected ApiBean.ApiDefinition newApiDefinition(final Method method, final ApplicationContext applicationContext) {
        return new ApiBean.ApiDefinition(method);
    }
    
    /**
     * Extracts the collection of instances of supported methods.<br>
     *
     * @param bean               bean instance.
     * @param applicationContext applicationContext.
     * @return bean map
     */
    protected List<Method> extractSupportMethods(final Object bean, final ApplicationContext applicationContext) {
        return Arrays.stream(ReflectionUtils.getUniqueDeclaredMethods(bean.getClass()))
                .collect(Collectors.toList());
    }
    
    /**
     * addExtractorProcessor.
     *
     * @param processor processor.
     */
    public void addExtractorProcessor(final ExtractorProcessor processor) {
        if (CollectionUtils.isEmpty(processor.supportedClient())) {
            return;
        }
        if (processor.supportedClient().contains(clientName())) {
            extractorProcessors.add(processor);
        }
    }
    
    /**
     * Extracts the collection of instances of supported beans.<br>
     * key is bean name<br>
     * value is bean instance.
     *
     * @param applicationContext applicationContext.
     * @return bean map
     */
    protected abstract Map<String, Object> extractSupportBeans(ApplicationContext applicationContext);
}

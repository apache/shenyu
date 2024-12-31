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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.matcher.ApiRegisterProcessor;
import org.springframework.beans.factory.InitializingBean;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * BaseApiRegistrarImpl.
 */
public abstract class BaseApiRegistrarImpl implements ApiRegistrar, InitializingBean {
    
    private final List<ApiRegisterProcessor> processors = new ArrayList<>();
    
    @Override
    public void afterPropertiesSet() {
        processors.sort(Comparator.comparingInt(ApiRegisterProcessor::order));
    }
    
    /**
     * add processor.
     *
     * @param processor processor
     */
    public void addApiProcessor(final ApiRegisterProcessor processor) {
        if (supportedType(processor)) {
            processors.add(processor);
            processors.sort(Comparator.comparingInt(ApiRegisterProcessor::order));
        }
    }
    
    @Override
    public void register(final ApiBean apiBean) {
        processors.forEach(processor -> processor.process(apiBean));
        if (ApiBean.Status.INIT.equals(apiBean.getStatus())) {
            // default register all api
            apiBean.setStatus(ApiBean.Status.REGISTRABLE_API);
            for (ApiBean.ApiDefinition definition : apiBean.getApiDefinitions()) {
                if (ApiBean.Status.INIT.equals(definition.getStatus())) {
                    definition.setStatus(ApiBean.Status.REGISTRABLE);
                }
            }
        }
        doRegister(apiBean);
    }
    
    protected void doRegister(final ApiBean apiBean) {
        if (ApiBean.Status.REGISTRABLE_BEAN.equals(apiBean.getStatus())) {
            doRegisterBean(apiBean);
        }
        
        if (ApiBean.Status.REGISTRABLE_API.equals(apiBean.getStatus())) {
            doRegisterApi(apiBean);
        }
        
        if (ApiBean.Status.REGISTRABLE.equals(apiBean.getStatus())) {
            doRegisterBean(apiBean);
            doRegisterApi(apiBean);
        }
    }
    
    private boolean supportedType(final ApiRegisterProcessor processor) {
        for (Class<?> type : processor.supportedRegisterDataType()) {
            if (type.isAssignableFrom(registerDataType())) {
                return true;
            }
        }
        return false;
    }
    
    private void doRegisterApi(final ApiBean apiBean) {
        for (ApiBean.ApiDefinition api : apiBean.getApiDefinitions()) {
            if (ApiBean.Status.REGISTRABLE.equals(api.getStatus())) {
                doRegisterApi(api);
            }
        }
    }
    
    /**
     * register api.
     *
     * @param api api
     */
    protected abstract void doRegisterApi(ApiBean.ApiDefinition api);
    
    /**
     * register bean.
     *
     * @param apiBean bean
     */
    protected abstract void doRegisterBean(ApiBean apiBean);
}

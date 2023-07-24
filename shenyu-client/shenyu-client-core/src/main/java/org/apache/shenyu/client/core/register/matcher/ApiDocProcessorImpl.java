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

package org.apache.shenyu.client.core.register.matcher;

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;

import java.util.List;

/**
 * ApiDocProcessorImpl.<br>
 * About support for {@link ApiDoc} annotations
 *
 * @see ApiDoc
 */
public class ApiDocProcessorImpl extends BaseAnnotationApiProcessor<ApiDoc> {
    
    @Override
    public void process(final ApiBean apiBean, final ApiDoc annotation) {
        apiBean.addProperties("desc", annotation.desc());
        apiBean.addProperties("tags", String.join(",", annotation.tags()));
        apiBean.setStatus(ApiBean.Status.REGISTRABLE_API);
    }
    
    @Override
    public void process(final ApiBean.ApiDefinition definition, final ApiDoc annotation) {
        definition.addProperties("desc", annotation.desc());
        definition.addProperties("tags", String.join(",", annotation.tags()));
        definition.setStatus(ApiBean.Status.REGISTRABLE);
    }
    
    @Override
    public Class<ApiDoc> matchAnnotation() {
        return ApiDoc.class;
    }
    
    @Override
    public List<Class<?>> supportedRegisterDataType() {
        return ListUtil.of(ApiDocRegisterDTO.class);
    }
}

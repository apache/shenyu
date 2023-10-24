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

package org.apache.shenyu.sdk.spring.annotation;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.springframework.web.bind.annotation.RequestBody;

import java.lang.annotation.Annotation;

/**
 * {@link RequestBody} parameter processor.
 */
public class RequestBodyParameterProcessor implements AnnotatedParameterProcessor {

    private static final Class<RequestBody> ANNOTATION = RequestBody.class;

    @Override
    public Class<? extends Annotation> getAnnotationType() {
        return ANNOTATION;
    }

    @Override
    public boolean processArgument(final ShenyuRequest shenyuRequest, final Annotation annotation, final Object arg) {
        if (arg instanceof String) {
            shenyuRequest.setBody((String) arg);
        } else {
            shenyuRequest.setBody(JsonUtils.toJson(arg));
        }
        return true;
    }

}

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

import org.apache.commons.lang3.RegExUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import static org.apache.shenyu.sdk.core.util.Util.checkState;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.springframework.web.bind.annotation.PathVariable;

import java.lang.annotation.Annotation;
import java.util.Map;

/**
 * {@link PathVariable} parameter processor.
 */
public class PathVariableParameterProcessor implements AnnotatedParameterProcessor {

    private static final Class<PathVariable> ANNOTATION = PathVariable.class;

    @Override
    public Class<? extends Annotation> getAnnotationType() {
        return ANNOTATION;
    }

    @Override
    public boolean processArgument(final ShenyuRequest shenyuRequest, final Annotation annotation, final Object arg) {
        String name = ANNOTATION.cast(annotation).value();
        RequestTemplate requestTemplate = shenyuRequest.getRequestTemplate();
        checkState(arg instanceof String && StringUtils.isNotBlank(name) || arg instanceof Map,
            "PathVariable Object class pls is String or Map<String, String> and PathVariable annotation value could not be empty when String class at the method %s.", requestTemplate.getMethod());

        if (arg instanceof String) {
            String varName = "{" + name + "}";
            String varNameRegex = "\\{" + name + "\\}";
            if (requestTemplate.getPath().contains(varName)) {
                shenyuRequest.setUrl(requestTemplate.getUrl() + RegExUtils.replaceAll(requestTemplate.getPath(), varNameRegex, String.valueOf(arg)));
            }
            return true;
        }
        String path = requestTemplate.getPath();
        for (Map.Entry<?, ?> entry : ((Map<?, ?>) arg).entrySet()) {
            final Object key = entry.getKey();
            final Object value = entry.getValue();
            if (key instanceof String && value instanceof String) {
                String varName = "{" + key + "}";
                String varNameRegex = "\\{" + key + "\\}";
                if (path.contains(varName)) {
                    path = RegExUtils.replaceAll(path, varNameRegex, (String) value);
                }
            }
        }
        shenyuRequest.setUrl(requestTemplate.getUrl() + path);
        return true;
    }
}

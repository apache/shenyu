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

package org.apache.shenyu.plugin.mock.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.expression.EvaluationContext;

/**
 * EvaluationContextUtil.
 */
public final class EvaluationContextUtil {
    
    private static final Logger LOG = LoggerFactory.getLogger(EvaluationContextUtil.class);
    
    private EvaluationContextUtil() {
    
    }
    
    /**
     * init EvaluationContext.
     *
     * @param context context
     */
    public static void init(final EvaluationContext context) {
        
        try {
            registerMockFunction(context, "double", "randomDouble", double.class, double.class, String[].class);
            
            registerMockFunction(context, "bool", "bool");
            
            registerMockFunction(context, "int", "randomInt", int.class, int.class);
            
            registerMockFunction(context, "email", "email");
            
            registerMockFunction(context, "phone", "phone");
            
            registerMockFunction(context, "zh", "zh", int.class, int.class);
            
            registerMockFunction(context, "en", "en", int.class, int.class);
            
            registerMockFunction(context, "oneOf", "oneOf", Object[].class);
            
            registerMockFunction(context, "current", "current", String[].class);
            
            registerMockFunction(context, "array", "array", Object.class, int.class);
            
            registerMockFunction(context, "nowDate", "nowDate");
            
            registerMockFunction(context, "nowTime", "nowTime");
            
        } catch (NoSuchMethodException e) {
            // It will never happen
            LOG.error(e.getMessage(), e);
        }
    }
    
    private static void registerMockFunction(final EvaluationContext context,
                                             final String name,
                                             final String methodName,
                                             final Class<?>... parameterTypes) throws NoSuchMethodException {
        context.setVariable(name, MockUtil.class.getDeclaredMethod(methodName, parameterTypes));
    }
}

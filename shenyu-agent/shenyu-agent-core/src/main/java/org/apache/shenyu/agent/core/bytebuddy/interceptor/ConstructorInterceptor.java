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
 *
 */

package org.apache.shenyu.agent.core.bytebuddy.interceptor;

import net.bytebuddy.implementation.bind.annotation.AllArguments;
import net.bytebuddy.implementation.bind.annotation.RuntimeType;
import net.bytebuddy.implementation.bind.annotation.This;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.ConstructorHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;


/**
 * The type Constructor interceptor.
 */
public class ConstructorInterceptor {
    
    private static final Logger LOG = LoggerFactory.getLogger(ConstructorInterceptor.class);
    
    private final List<ConstructorHandler> handlerList;
    
    /**
     * Instantiates a new Constructor interceptor.
     *
     * @param handlerList the handler list
     */
    public ConstructorInterceptor(final List<ConstructorHandler> handlerList) {
        this.handlerList = handlerList;
    }
    
    /**
     * Intercept.
     *
     * @param target the target
     * @param args the args
     */
    @RuntimeType
    public void intercept(@This final TargetObject target, @AllArguments final Object[] args) {
        for (ConstructorHandler handler : handlerList) {
            try {
                handler.onConstructor(target, args);
                // CHECKSTYLE:OFF
            } catch (final Throwable throwable) {
                // CHECKSTYLE:ON
                LOG.error("Constructor advice execution error. class: {}", target.getClass().getTypeName(), throwable);
            }
        }
    }
}

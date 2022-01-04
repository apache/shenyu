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

package org.apache.shenyu.agent.api.handler;

import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;

import java.lang.reflect.Method;

/**
 * The interface Class instance method handler.
 */
public interface InstanceMethodHandler {
    
    /**
     * Before.
     *
     * @param target the target
     * @param method the method
     * @param args the args
     * @param result the result
     */
    default void before(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
    }
    
    /**
     * After.
     *
     * @param target the target
     * @param method the method
     * @param args the args
     * @param methodResult {@linkplain MethodResult}
     * @param result the result
     * @return result
     */
    default Object after(final TargetObject target, final Method method, final Object[] args, final MethodResult methodResult, final Object result) {
        return result;
    }
    
    /**
     * On throwing.
     *
     * @param target the target
     * @param method the method
     * @param args the args
     * @param throwable the throwable
     */
    default void onThrowing(final TargetObject target, final Method method, final Object[] args, final Throwable throwable) {
    }
}

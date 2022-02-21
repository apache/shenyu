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
import net.bytebuddy.implementation.bind.annotation.Origin;
import net.bytebuddy.implementation.bind.annotation.RuntimeType;
import net.bytebuddy.implementation.bind.annotation.SuperCall;
import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.handler.StaticMethodHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * The type Static method interceptor.
 */
public class StaticMethodInterceptor {
    
    private static final Logger LOG = LoggerFactory.getLogger(StaticMethodInterceptor.class);
    
    private final List<StaticMethodHandler> handlerList;
    
    /**
     * Instantiates a new Static method interceptor.
     *
     * @param handlerList the handler list
     */
    public StaticMethodInterceptor(final List<StaticMethodHandler> handlerList) {
        this.handlerList = handlerList;
    }
    
    /**
     * Intercept object.
     *
     * @param klass the klass
     * @param method the method
     * @param args the args
     * @param callable the callable
     * @return the object
     * @throws Exception the exception
     */
    @RuntimeType
    public Object intercept(@Origin final Class<?> klass, @Origin final Method method, @AllArguments final Object[] args, @SuperCall final Callable<?> callable) throws Exception {
        Object result = null;
        for (StaticMethodHandler handler : handlerList) {
            MethodResult methodResult = new MethodResult();
            try {
                handler.before(klass, method, args, methodResult);
                // CHECKSTYLE:OFF
            } catch (final Throwable ex) {
                // CHECKSTYLE:ON
                LOG.error("Failed to execute the before method of method {} in class {}", method.getName(), klass, ex);
            }
            try {
                result = callable.call();
                // CHECKSTYLE:OFF
            } catch (final Throwable ex) {
                // CHECKSTYLE:ON
                try {
                    handler.onThrowing(klass, method, args, ex);
                    // CHECKSTYLE:OFF
                } catch (final Throwable ignored) {
                    // CHECKSTYLE:ON
                    LOG.error("Failed to execute the error handler of method {} in class {}", method.getName(), klass, ex);
                    throw ex;
                }
            } finally {
                try {
                    handler.after(klass, method, args, methodResult);
                    // CHECKSTYLE:OFF
                } catch (final Throwable ex) {
                    // CHECKSTYLE:ON
                    LOG.error("Failed to execute the after method of method {} in class {}", method.getName(), klass, ex);
                }
            }
        }
        return result;
    }
}

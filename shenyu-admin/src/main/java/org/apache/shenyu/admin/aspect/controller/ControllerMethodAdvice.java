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

package org.apache.shenyu.admin.aspect.controller;

import com.google.common.base.Stopwatch;
import org.apache.shenyu.common.exception.ShenyuException;

import java.lang.reflect.Method;

/**
 * ControllerMethodAdvice.
 */
public interface ControllerMethodAdvice {
    
    /**
     * Before the method executes.<br>
     * If you want to prevent the method from executing, throw an exception in the method<br>
     *
     * @param bean      controller instance
     * @param method    method
     * @param stopwatch stopwatch
     * @see ShenyuException
     */
    default void doPreProcess(Object bean, Method method, Stopwatch stopwatch) {
        // nothing
    }
    
    /**
     * method error.<br>
     *
     * @param bean      controller instance
     * @param method    method
     * @param stopwatch stopwatch
     * @param throwable throwable
     */
    default void doThrowable(Object bean, Method method, Stopwatch stopwatch, Throwable throwable) {
        // skip
    }
    
    /**
     * method after.<br>
     *
     * @param bean      controller instance
     * @param method    method
     * @param stopwatch stopwatch
     */
    default void doFinally(Object bean, Method method, Stopwatch stopwatch) {
        // nothing
    }
}

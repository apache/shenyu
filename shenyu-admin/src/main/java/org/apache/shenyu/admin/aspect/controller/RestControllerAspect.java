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
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.List;

/**
 * RestControllerAspect.
 */
@Aspect
@Component
public class RestControllerAspect {
    
    private final List<ControllerMethodAdvice> methodAdviceList;
    
    public RestControllerAspect(final List<ControllerMethodAdvice> methodAdviceList) {
        this.methodAdviceList = methodAdviceList;
    }
    
    /**
     * cut.
     */
    @Pointcut("@within(org.springframework.web.bind.annotation.RestController) "
            + "|| @within(org.apache.shenyu.admin.aspect.annotation.RestApi)")
    public void controller() {
    }
    
    /**
     * controller.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     * @throws ShenyuException Throwable
     */
    @Around("controller()")
    public Object logAround(final ProceedingJoinPoint point) {
        final Stopwatch stopwatch = Stopwatch.createStarted();
        final Method method = ((MethodSignature) point.getSignature()).getMethod();
        final Object target = point.getTarget();
        try {
            doExec(a -> a.doPreProcess(target, method, stopwatch));
            return point.proceed();
        } catch (final Throwable throwable) {
            doExec(a -> a.doThrowable(target, method, stopwatch, throwable));
            if (throwable instanceof ShenyuException) {
                throw (ShenyuException) throwable;
            }
            throw new ShenyuAdminException(throwable);
        } finally {
            try {
                doExec(a -> a.doFinally(target, method, stopwatch));
            } finally {
                SessionUtil.clean();
            }
        }
    }
    
    void doExec(final Call call) {
        for (ControllerMethodAdvice advice : methodAdviceList) {
            call.call(advice);
        }
    }
    
    interface Call {
        
        /**
         * call.
         *
         * @param advice advice
         */
        void call(ControllerMethodAdvice advice);
    }
    
}

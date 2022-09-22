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

package org.apache.shenyu.examples.common.aop;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Aspect
public class LogInterceptor {

    private static final Logger log = LoggerFactory.getLogger(LogInterceptor.class);

    /**
     * log annotation pointcut.
     */
    @Pointcut("@annotation(org.apache.shenyu.examples.common.aop.Log)")
    public void logPointcut() {
        //just for pointcut
    }

    /**
     * define log pointcut with around advice.
     * @param point joinPoint
     * @return the result of proceeding
     */
    @Around("logPointcut()")
    public Object around(final ProceedingJoinPoint point) {
        try {
            log.info("before");
            return point.proceed(point.getArgs());
        } catch (Throwable throwable) {
            log.error("log point cut throw exception:", throwable);
        } finally {
            log.info("after");
        }
        return null;
    }
}

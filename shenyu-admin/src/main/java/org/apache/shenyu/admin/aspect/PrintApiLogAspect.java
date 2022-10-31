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

package org.apache.shenyu.admin.aspect;

import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * print log aspect.
 */
@Aspect
@Component
public class PrintApiLogAspect {
    
    private static final Logger LOG = LoggerFactory.getLogger(PrintApiLogAspect.class);
    
    private final DashboardProperties properties;
    
    public PrintApiLogAspect(final DashboardProperties properties) {
        this.properties = properties;
    }
    
    /**
     * cut.
     */
    @Pointcut("@within(org.springframework.web.bind.annotation.RestController)")
    public void pointCut() {
    }
    
    /**
     * log around.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     */
    @Around("pointCut()")
    public Object logAround(final ProceedingJoinPoint point) {
        final long start = System.currentTimeMillis();
        try {
            preLog(point);
            return point.proceed();
        } catch (final Throwable throwable) {
            throw new ShenyuException(throwable);
        } finally {
            postLog(point, start);
            SessionUtil.clean();
        }
    }
    
    private void postLog(final ProceedingJoinPoint point, final long start) {
        if (Boolean.TRUE.equals(properties.getEnablePrintApiLog())) {
            LOG.info("{} exec: method [{}.{}] over, time cost: {}", SessionUtil.visitorName(),
                    point.getTarget().getClass().getSimpleName(), point.getSignature().getName(), System.currentTimeMillis() - start);
        }
    }
    
    private void preLog(final ProceedingJoinPoint point) {
        if (Boolean.TRUE.equals(properties.getEnablePrintApiLog())) {
            LOG.info("{} exec: method [{}.{}]", SessionUtil.visitorName(), point.getTarget().getClass().getSimpleName(), point.getSignature().getName());
        }
    }
    
}

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
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.admin.utils.SessionUtil.visitorName;

/**
 * PrintLogControllerMethodAdviceImpl.
 */
@Component
public class PrintLogControllerMethodAdviceImpl implements ControllerMethodAdvice {
    
    private static final Logger LOG = LoggerFactory.getLogger(PrintLogControllerMethodAdviceImpl.class);
    
    private final DashboardProperties properties;
    
    public PrintLogControllerMethodAdviceImpl(final DashboardProperties properties) {
        this.properties = properties;
    }
    
    @Override
    public void doPreProcess(final Object bean, final Method method, final Stopwatch stopwatch) {
        if (Boolean.TRUE.equals(properties.getEnablePrintApiLog())) {
            LOG.info("{} exec: method [{}.{}]", visitorName(), bean.getClass().getSimpleName(), method.getName());
        }
    }
    
    @Override
    public void doFinally(final Object bean, final Method method, final Stopwatch stopwatch) {
        if (Boolean.TRUE.equals(properties.getEnablePrintApiLog())) {
            LOG.info("{} exec: method [{}.{}] over, time cost: {}", visitorName(),
                    bean.getClass().getSimpleName(), method.getName(), stopwatch.elapsed(TimeUnit.MILLISECONDS));
        }
    }
}

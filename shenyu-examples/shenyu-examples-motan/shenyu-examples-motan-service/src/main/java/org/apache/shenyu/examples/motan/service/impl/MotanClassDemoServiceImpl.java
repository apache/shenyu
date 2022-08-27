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

package org.apache.shenyu.examples.motan.service.impl;

import com.weibo.api.motan.config.springsupport.annotation.MotanService;
import org.apache.shenyu.client.motan.common.annotation.ShenyuMotanClient;
import org.apache.shenyu.examples.motan.service.MotanClassDemoService;
import org.apache.shenyu.springboot.starter.client.motan.ShenyuMotanClientConfiguration;

/**
 * Motan Class demo service.
 *
 * <P>Default motan service name is "motan2", If you want to inject other services,
 * please refer to {@link ShenyuMotanClientConfiguration}
 */
@MotanService(export = "motan2:8002")
@ShenyuMotanClient("/demoTest/**")
public class MotanClassDemoServiceImpl implements MotanClassDemoService {

    @Override
    public String hello(final String name) {
        return "hello " + name;
    }

    @Override
    public String testTimeOut(final long timeout) {
        try {
            Thread.sleep(timeout * 1000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return "hello timeout " + timeout + "s";
    }
}

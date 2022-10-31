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

package org.apache.shenyu.examples.tars.servant.testapp.impl;

import com.qq.tars.spring.annotation.TarsServant;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsClient;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsService;
import org.apache.shenyu.examples.tars.servant.testapp.OrderServant;

@TarsServant("OrderObj")
@ShenyuTarsClient("/order")
@ShenyuTarsService(serviceName = "ShenyuExampleServer.ShenyuExampleApp.OrderObj")
public class OrderServantImpl implements OrderServant {

    @Override
    @ShenyuTarsClient("/hello")
    public String hello(final int no, final String name) {
        return String.format("hello no=%s, name=%s, time=%s", no, name, System.currentTimeMillis());
    }

    @Override
    @ShenyuTarsClient("/helloInt")
    public int helloInt(final int no, final String name) {
        return 1;
    }
}

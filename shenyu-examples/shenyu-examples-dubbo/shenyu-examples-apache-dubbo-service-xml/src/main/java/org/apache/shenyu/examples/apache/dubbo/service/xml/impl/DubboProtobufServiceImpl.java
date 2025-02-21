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

package org.apache.shenyu.examples.apache.dubbo.service.xml.impl;

import com.google.protobuf.Empty;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.examples.dubbo.api.service.DubboProtobufService;
import org.apache.shenyu.examples.dubbo.api.service.DubboTestProtobuf;
import org.springframework.stereotype.Service;

@Service("dubboProtobufService")
@ShenyuDubboClient(value = "/protobufSerialization")
public class DubboProtobufServiceImpl implements DubboProtobufService {

    @ShenyuDubboClient("/insert")
    @Override
    public DubboTestProtobuf insert(final DubboTestProtobuf request) {
        return request;
    }

    @ShenyuDubboClient("/update")
    @Override
    public Empty update(final DubboTestProtobuf request) {
        return Empty.getDefaultInstance();
    }

    @ShenyuDubboClient("/findOne")
    @Override
    public DubboTestProtobuf findOne(final Empty request) {
        return DubboTestProtobuf.newBuilder().setId("1").setName("test1").build();
    }
}

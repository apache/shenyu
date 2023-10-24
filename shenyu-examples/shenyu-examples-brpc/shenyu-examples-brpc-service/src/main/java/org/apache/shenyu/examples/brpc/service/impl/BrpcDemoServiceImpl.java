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

package org.apache.shenyu.examples.brpc.service.impl;

import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcClient;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcService;
import org.apache.shenyu.examples.brpc.api.entity.Address;
import org.apache.shenyu.examples.brpc.api.entity.ExtInfo;
import org.apache.shenyu.examples.brpc.api.entity.Gender;
import org.apache.shenyu.examples.brpc.api.entity.User;
import org.apache.shenyu.examples.brpc.api.service.BrpcDemoService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Brpc demo service.
 */
@ShenyuBrpcService
public class BrpcDemoServiceImpl implements BrpcDemoService {

    private static final Logger LOG = LoggerFactory.getLogger(BrpcDemoServiceImpl.class);

    @Override
    @ShenyuBrpcClient("/getUser")
    public User getUser(final Long userId) {
        User user = new User();
        user.setUserId(userId);
        user.setUserName("User1");
        user.setBalance(1000.21d);
        user.setGender(Gender.MALE);
        List<String> tags = new LinkedList<>();
        tags.add("zst");
        tags.add("127072");
        user.setTags(tags);
        List<ExtInfo> extInfos = new LinkedList<>();
        ExtInfo extInfo = new ExtInfo("hobby", "game");
        extInfos.add(extInfo);
        user.setExtInfos(extInfos);
        user.setMap(Collections.singletonMap("key", new Address("HZ")));
        return user;
    }

    @Override
    @ShenyuBrpcClient(path = "/allName")
    public List<String> allName() {
        List<String> users = new LinkedList<>();
        users.add("zst");
        users.add("hwj");
        return users;
    }

    @Override
    @ShenyuBrpcClient(path = "/getUserByIdAndName")
    public User getUserByIdAndName(final Long userId, final String name) {
        User user = new User();
        user.setUserId(userId);
        user.setUserName("name");
        return user;
    }

    @Override
    @ShenyuBrpcClient(path = "/userMap")
    public Map<Long, User> userMap(final Long userId) {
        User user = new User();
        user.setUserId(2L);
        user.setUserName("u2");
        user.setBalance(1000.21d);
        List<String> tags2 = new LinkedList<>();
        tags2.add("fgh");
        tags2.add("123123");
        user.setTags(tags2);
        List<ExtInfo> extInfos2 = new LinkedList<>();
        ExtInfo extInfo2 = new ExtInfo("hobby", "learn");
        extInfos2.add(extInfo2);
        user.setExtInfos(extInfos2);
        user.setMap(Collections.singletonMap("Key1", new Address("Beijing")));

        return Collections.singletonMap(2L, user);
    }

    @Override
    @ShenyuBrpcClient("/connect")
    public void connect() {
        LOG.info("Connect Success");
    }
}

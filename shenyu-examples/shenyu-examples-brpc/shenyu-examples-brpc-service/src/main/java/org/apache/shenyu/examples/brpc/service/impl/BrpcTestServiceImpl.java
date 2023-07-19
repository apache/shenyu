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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcClient;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcService;
import org.apache.shenyu.examples.brpc.api.entity.BigObject;
import org.apache.shenyu.examples.brpc.api.entity.ComplexObjects;
import org.apache.shenyu.examples.brpc.api.entity.User;
import org.apache.shenyu.examples.brpc.api.service.BrpcTestService;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Brpc test service.
 */
@ShenyuBrpcService
public class BrpcTestServiceImpl implements BrpcTestService {

    private static final Integer SIZE = 20000;

    @Override
    @ShenyuBrpcClient("/getArray")
    public String[] getArray(final String[] param) {
        return param;
    }

    @Override
    @ShenyuBrpcClient("/getUsersList0")
    public List<User> getUsersList0(final List<User> inputUsers) {
        if (CollectionUtils.isNotEmpty(inputUsers)) {
            return inputUsers;
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    @ShenyuBrpcClient("/getUserByObj")
    public User getUserByObj(final User inputUser) {
        return Optional.ofNullable(inputUser).orElse(new User());
    }

    @Override
    @ShenyuBrpcClient("/getUsersList1")
    public List<User> getUsersList1(final List<User> inputUsers, final String[] param) {
        if (CollectionUtils.isNotEmpty(inputUsers)) {
            return inputUsers;
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    @ShenyuBrpcClient("/bigObject0")
    public BigObject bigObject0() {
        BigObject bigObject = new BigObject();
        bigObject.setId(1);
        String[] obj = new String[SIZE];
        for (int i = 0; i < SIZE; i++) {
            obj[i] = String.valueOf(i);
        }
        bigObject.setObj(obj);
        return bigObject;
    }

    @Override
    @ShenyuBrpcClient("/complexObjects")
    public ComplexObjects complexObjects(final User user) {
        User newUser = Optional.ofNullable(user).orElse(new User());
        newUser.setUserId(1L);
        newUser.setUserName("new user");
        ComplexObjects complexObjects = new ComplexObjects();
        complexObjects.setUser(user);
        BigObject bigObject = new BigObject();
        bigObject.setId(1);
        String[] obj = new String[SIZE];
        for (int i = 0; i < SIZE; i++) {
            obj[i] = String.valueOf(i);
        }
        bigObject.setObj(obj);
        complexObjects.setBigObject(bigObject);
        return complexObjects;
    }

}

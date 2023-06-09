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

    @Override
    public String[] getArray(final String[] param) {
        return param;
    }

    @Override
    public List<User> getUsers(final List<User> inputUsers) {
        if (CollectionUtils.isNotEmpty(inputUsers)) {
            return inputUsers;
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    public List<User> getUsers(final List<User> inputUsers, final String[] param) {
        if (CollectionUtils.isNotEmpty(inputUsers)) {
            return inputUsers;
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    public BigObject bigObject() {
        return new BigObject(new Byte[BigObject.MB * 100]);
    }

    @Override
    public BigObject bigObject(final BigObject bigObject) {
        return new BigObject(new Byte[BigObject.GB]);
    }

    @Override
    public ComplexObjects complexObjects(final User user) {
        User newUser = Optional.ofNullable(user).orElse(new User());
        newUser.setUserId(100L);
        newUser.setUserName("new user");
        ComplexObjects complexObjects = new ComplexObjects();
        complexObjects.setUser(user);
        complexObjects.setBigObject(new BigObject(new Byte[BigObject.GB]));
        return complexObjects;
    }

}

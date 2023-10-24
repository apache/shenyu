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

package org.apache.shenyu.examples.brpc.api.service;

import org.apache.shenyu.examples.brpc.api.entity.User;

import java.util.List;
import java.util.Map;

public interface BrpcDemoService {

    /**
     * connect.
     */
    void connect();

    /**
     * get user.
     *
     * @param userId user id
     * @return user
     */
    User getUser(Long userId);

    /**
     * get all name.
     *
     * @return list of name
     */
    List<String> allName();

    /**
     * get user by id and name.
     *
     * @param userId user id
     * @param name user name
     * @return user
     */
    User getUserByIdAndName(Long userId, String name);

    /**
     * get user map.
     * @param userId user id
     * @return user map
     */
    Map<Long, User> userMap(Long userId);
}

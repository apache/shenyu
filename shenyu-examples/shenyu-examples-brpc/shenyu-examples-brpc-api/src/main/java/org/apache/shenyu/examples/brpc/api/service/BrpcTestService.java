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

import org.apache.shenyu.examples.brpc.api.entity.BigObject;
import org.apache.shenyu.examples.brpc.api.entity.ComplexObjects;
import org.apache.shenyu.examples.brpc.api.entity.User;

import java.util.List;

public interface BrpcTestService {

    /**
     * get array.
     *
     * @param param param
     * @return array
     */
    String[] getArray(String[] param);


    /**
     * getUserByObj.
     *
     * @param inputUser input user
     * @return user
     */
    User getUserByObj(User inputUser);

    /**
     * get users.
     *
     * @param inputUsers input users
     * @return users
     */
    List<User> getUsersList0(List<User> inputUsers);

    /**
     * get users.
     *
     * @param inputUsers input users
     * @param param      param
     * @return users
     */
    List<User> getUsersList1(List<User> inputUsers, String[] param);

    /**
     * big objects.
     *
     * @return big objects
     */
    BigObject bigObject0();

    /**
     * big objects.
     *
     * @param user user
     * @return big objects
     */
    ComplexObjects complexObjects(User user);

}

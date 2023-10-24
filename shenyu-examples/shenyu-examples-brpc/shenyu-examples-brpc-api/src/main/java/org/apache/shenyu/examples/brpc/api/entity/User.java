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

package org.apache.shenyu.examples.brpc.api.entity;

import java.util.List;
import java.util.Map;

public class User {

    private long userId;

    private String userName;

    private double balance;

    private List<String> tags;

    private Map<String, Address> map;

    private List<ExtInfo> extInfos;

    private Gender gender;

    public User() {

    }

    /**
     * get user id.
     *
     * @return user id
     */
    public long getUserId() {
        return userId;
    }

    /**
     * set user id.
     *
     * @param userId user id
     */
    public void setUserId(final long userId) {
        this.userId = userId;
    }

    /**
     * get user name.
     *
     * @return user name
     */
    public String getUserName() {
        return userName;
    }

    /**
     * set user name.
     *
     * @param userName user name
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }

    /**
     * get balance.
     *
     * @return balance.
     */
    public double getBalance() {
        return balance;
    }

    /**
     * set balance.
     *
     * @param balance balance
     */
    public void setBalance(final double balance) {
        this.balance = balance;
    }

    /**
     * get tag list.
     *
     * @return list of tag
     */
    public List<String> getTags() {
        return tags;
    }

    /**
     * set tag list.
     *
     * @param tags tags
     */
    public void setTags(final List<String> tags) {
        this.tags = tags;
    }

    /**
     * set ext info list.
     *
     * @param extInfos exinfos
     */
    public void setExtInfos(final List<ExtInfo> extInfos) {
        this.extInfos = extInfos;
    }

    /**
     * set gender.
     *
     * @param gender gender
     */
    public void setGender(final Gender gender) {
        this.gender = gender;
    }

    /**
     * set map.
     *
     * @param map map
     */
    public void setMap(final Map<String, Address> map) {
        this.map = map;
    }
}

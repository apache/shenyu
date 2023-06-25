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

package org.apache.shenyu.integratedtest.common.dto;

/**
 * ComplexObjects.
 */
public class ComplexObjects {

    private BigObject bigObject;

    private BrpcTest user;

    /**
     * getBigObject.
     *
     * @return BigObject
     */
    public BigObject getBigObject() {
        return bigObject;
    }

    /**
     * setBigObject.
     *
     * @param bigObject big object
     */
    public void setBigObject(final BigObject bigObject) {
        this.bigObject = bigObject;
    }

    /**
     * getUser.
     *
     * @return User
     */
    public BrpcTest getUser() {
        return user;
    }

    /**
     * setUser.
     *
     * @param user User
     */
    public void setUser(final BrpcTest user) {
        this.user = user;
    }
}

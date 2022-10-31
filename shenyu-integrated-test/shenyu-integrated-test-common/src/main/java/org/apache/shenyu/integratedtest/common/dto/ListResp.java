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

import java.util.List;

public class ListResp {

    private Integer total;

    private List<DubboTest> users;

    public ListResp() {
    }

    public ListResp(final Integer total, final List<DubboTest> users) {
        this.total = total;
        this.users = users;
    }

    /**
     * Get total.
     *
     * @return total
     */
    public Integer getTotal() {
        return total;
    }

    /**
     * Set total.
     *
     * @param total total
     */
    public void setTotal(final Integer total) {
        this.total = total;
    }

    /**
     * Get users.
     *
     * @return users
     */
    public List<DubboTest> getUsers() {
        return users;
    }

    /**
     * Set users.
     *
     * @param users users
     */
    public void setUsers(final List<DubboTest> users) {
        this.users = users;
    }

    @Override
    public String toString() {
        return "ListResp{" + "total=" + total + ", users=" + users + '}';
    }

}

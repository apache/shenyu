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

/**
 * big object.
 */
public class BigObject {

    public static final int MB = 1024 * 1024;

    public static final int GB = 1024 * 1024 * 1024;

    private Byte[] obj;

    /**
     * constructor.
     *
     * @param obj obj
     */
    public BigObject(final Byte[] obj) {
        this.obj = obj;
    }


    /**
     * get obj.
     *
     * @return Byte[]
     */
    public Byte[] getObj() {
        return obj;
    }

    /**
     * set obj.
     *
     * @param obj obj
     */
    public void setObj(final Byte[] obj) {
        this.obj = obj;
    }

}

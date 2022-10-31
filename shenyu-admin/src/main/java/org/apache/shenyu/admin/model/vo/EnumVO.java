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

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;

/**
 * this is enum view to web front.
 */
public class EnumVO implements Serializable {

    private static final long serialVersionUID = 213412846786447233L;

    /**
     * enum code.
     */
    private Object code;

    /**
     * enum name.
     */
    private String name;

    /**
     * whether support.
     */
    private Boolean support;

    public EnumVO() {
    }

    public EnumVO(final Object code, final String name, final Boolean support) {
        this.code = code;
        this.name = name;
        this.support = support;
    }

    /**
     * Gets the value of code.
     *
     * @return the value of code
     */
    public Object getCode() {
        return code;
    }

    /**
     * Sets the code.
     *
     * @param code code
     */
    public void setCode(final Object code) {
        this.code = code;
    }

    /**
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the value of support.
     *
     * @return the value of support
     */
    public Boolean getSupport() {
        return support;
    }

    /**
     * Sets the support.
     *
     * @param support support
     */
    public void setSupport(final Boolean support) {
        this.support = support;
    }
}

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

package org.apache.shenyu.plugin.tars.proxy;

import java.lang.reflect.Method;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Tars path invoke metadata.
 */
public class TarsInvokePrxList {

    private CopyOnWriteArrayList<TarsInvokePrx> tarsInvokePrxList;

    private Method method;

    private Class<?>[] paramTypes;

    private String[] paramNames;

    /**
     * Instantiates a new Tars invoke prx list.
     */
    public TarsInvokePrxList() {
    }

    /**
     * Instantiates a new Tars invoke prx list.
     *
     * @param tarsInvokePrxList the tars invoke prx list
     * @param method            the method
     * @param paramTypes        the param types
     * @param paramNames        the param names
     */
    public TarsInvokePrxList(final CopyOnWriteArrayList<TarsInvokePrx> tarsInvokePrxList,
                             final Method method,
                             final Class<?>[] paramTypes,
                             final String[] paramNames) {
        this.tarsInvokePrxList = tarsInvokePrxList;
        this.method = method;
        this.paramTypes = paramTypes;
        this.paramNames = paramNames;
    }

    /**
     * Gets tars invoke prx list.
     *
     * @return the tars invoke prx list
     */
    public CopyOnWriteArrayList<TarsInvokePrx> getTarsInvokePrxList() {
        return tarsInvokePrxList;
    }

    /**
     * Sets tars invoke prx list.
     *
     * @param tarsInvokePrxList the tars invoke prx list
     */
    public void setTarsInvokePrxList(final CopyOnWriteArrayList<TarsInvokePrx> tarsInvokePrxList) {
        this.tarsInvokePrxList = tarsInvokePrxList;
    }

    /**
     * Gets method.
     *
     * @return the method
     */
    public Method getMethod() {
        return method;
    }

    /**
     * Sets method.
     *
     * @param method the method
     */
    public void setMethod(final Method method) {
        this.method = method;
    }

    /**
     * Get param types class [ ].
     *
     * @return the class [ ]
     */
    public Class<?>[] getParamTypes() {
        return paramTypes;
    }

    /**
     * Sets param types.
     *
     * @param paramTypes the param types
     */
    public void setParamTypes(final Class<?>[] paramTypes) {
        this.paramTypes = paramTypes;
    }

    /**
     * Get param names string [ ].
     *
     * @return the string [ ]
     */
    public String[] getParamNames() {
        return paramNames;
    }

    /**
     * Sets param names.
     *
     * @param paramNames the param names
     */
    public void setParamNames(final String[] paramNames) {
        this.paramNames = paramNames;
    }
}

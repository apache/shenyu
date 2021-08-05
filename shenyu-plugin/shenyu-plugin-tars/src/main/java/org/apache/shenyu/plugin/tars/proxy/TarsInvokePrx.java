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

/**
 * Tars path invoke metadata.
 */
public class TarsInvokePrx {
    
    private Object invokePrx;

    private String host;

    /**
     * Instantiates a new Tars invoke prx.
     */
    public TarsInvokePrx() {
    }

    /**
     * Instantiates a new Tars invoke prx.
     *
     * @param invokePrx the invoke prx
     * @param host      the host
     */
    public TarsInvokePrx(final Object invokePrx, final String host) {
        this.invokePrx = invokePrx;
        this.host = host;
    }

    /**
     * Gets invoke prx.
     *
     * @return the invoke prx
     */
    public Object getInvokePrx() {
        return invokePrx;
    }

    /**
     * Sets invoke prx.
     *
     * @param invokePrx the invoke prx
     */
    public void setInvokePrx(final Object invokePrx) {
        this.invokePrx = invokePrx;
    }

    /**
     * Gets host.
     *
     * @return the host
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets host.
     *
     * @param host the host
     */
    public void setHost(final String host) {
        this.host = host;
    }
}

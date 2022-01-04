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

package org.apache.shenyu.agent.api.entity;

/**
 * The type Method result.
 */
public final class MethodResult {
    
    private boolean reset;
    
    private Object result;
    
    /**
     * Reset.
     *
     * @param result the result
     */
    public void reset(final Object result) {
        reset = true;
        this.result = result;
    }
    
    /**
     * Is reset boolean.
     *
     * @return the boolean
     */
    public boolean isReset() {
        return reset;
    }
    
    /**
     * Sets reset.
     *
     * @param reset the reset
     */
    public void setReset(final boolean reset) {
        this.reset = reset;
    }
    
    /**
     * Gets result.
     *
     * @return the result
     */
    public Object getResult() {
        return result;
    }
    
    /**
     * Sets result.
     *
     * @param result the result
     */
    public void setResult(final Object result) {
        this.result = result;
    }
}

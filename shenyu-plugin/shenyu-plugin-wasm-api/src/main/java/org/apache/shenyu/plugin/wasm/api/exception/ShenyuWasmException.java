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

package org.apache.shenyu.plugin.wasm.api.exception;

import org.apache.shenyu.common.exception.ShenyuException;

/**
 * Shenyu WASM Exception.
 */
public class ShenyuWasmException extends ShenyuException {
    
    private static final long serialVersionUID = -3927608941632834099L;
    
    /**
     * Instantiates a new Shenyu WASM exception.
     *
     * @param e the e
     */
    public ShenyuWasmException(final Throwable e) {
        super(e);
    }
    
    /**
     * Instantiates a new Shenyu WASM exception.
     *
     * @param message the message
     */
    public ShenyuWasmException(final String message) {
        super(message);
    }
    
    /**
     * Instantiates a new Shenyu WASM exception.
     *
     * @param message   the message
     * @param throwable the throwable
     */
    public ShenyuWasmException(final String message, final Throwable throwable) {
        super(message, throwable);
    }
}

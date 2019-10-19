/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.stroe.api.exception;

import org.dromara.soul.common.exception.SoulException;

/**
 * The SoulException .
 *
 * @author xiaoyu
 */
public class StoreException extends SoulException {

    /**
     * Instantiates a new Store exception.
     *
     * @param e the e
     */
    public StoreException(Throwable e) {
        super(e);
    }

    /**
     * Instantiates a new Store exception.
     *
     * @param message the message
     */
    public StoreException(String message) {
        super(message);
    }

    /**
     * Instantiates a new Store exception.
     *
     * @param message   the message
     * @param throwable the throwable
     */
    public StoreException(String message, Throwable throwable) {
        super(message, throwable);
    }
}

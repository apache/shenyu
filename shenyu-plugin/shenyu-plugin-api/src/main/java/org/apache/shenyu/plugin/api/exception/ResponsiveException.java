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

package org.apache.shenyu.plugin.api.exception;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.springframework.web.server.ServerWebExchange;

public class ResponsiveException extends ShenyuException {

    private final int code;

    private final ServerWebExchange webExchange;

    public ResponsiveException(final int code, final String message, final ServerWebExchange webExchange) {
        super(message);
        this.code = code;
        this.webExchange = webExchange;
    }

    public ResponsiveException(final ShenyuResultEnum shenyuResult, final ServerWebExchange webExchange) {
        super(shenyuResult.getMsg());
        this.code = shenyuResult.getCode();
        this.webExchange = webExchange;
    }

    /** Gets code.
     * @return code
     */
    public int getCode() {
        return code;
    }

    /** Gets serverWebExchange.
     * @return serverWebExchange
     */
    public ServerWebExchange getWebExchange() {
        return webExchange;
    }
}

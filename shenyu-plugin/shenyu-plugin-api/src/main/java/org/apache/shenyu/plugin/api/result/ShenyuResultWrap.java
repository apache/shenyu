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

package org.apache.shenyu.plugin.api.result;

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.web.server.ServerWebExchange;

/**
 * The type shenyu result warp.
 */
public final class ShenyuResultWrap {
    
    private ShenyuResultWrap() {
    }
    
    /**
     * Success object.
     *
     * @param exchange the exchange
     * @param object  the object
     * @return the success object
     */
    public static Object success(final ServerWebExchange exchange, final Object object) {
        return shenyuResult().result(exchange, object);
    }

    /**
     * Error object.
     *
     * @param exchange the exchange
     * @param shenyuResult  the shenyuResult
     * @param object  the object
     * @return the object
     */
    public static Object error(final ServerWebExchange exchange, final ShenyuResultEnum shenyuResult, final Object object) {
        return shenyuResult().error(exchange, shenyuResult.getCode(), shenyuResult.getMsg(), object);
    }
    
    /**
     * Error object.
     *
     * @param shenyuResult the shenyuResult
     * @param object  the object
     * @return the object
     */
    public static Object error(final ShenyuResultEnum shenyuResult, final Object object) {
        return shenyuResult().error(shenyuResult.getCode(), shenyuResult.getMsg(), object);
    }
    
    /**
     * Error object.
     *
     * @param exchange the exchange
     * @param shenyuResult the shenyuResult
     * @return the object
     */
    public static Object error(final ServerWebExchange exchange, final ShenyuResultEnum shenyuResult) {
        return shenyuResult().error(exchange, shenyuResult.getCode(), shenyuResult.getMsg(), null);
    }

    /**
     * Error object.
     *
     * @param exchange the exchange
     * @param code    the code
     * @param message the message
     * @param object  the object
     * @return the object
     */
    public static Object error(final ServerWebExchange exchange, final int code, final String message, final Object object) {
        return shenyuResult().error(exchange, code, message, object);
    }

    /**
     * shenyu result bean.
     *
     * @return the shenyu result bean.
     */
    public static ShenyuResult<?> shenyuResult() {
        return SpringBeanUtils.getInstance().getBean(ShenyuResult.class);
    }
}

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

package org.apache.shenyu.sdk.core.interceptor;

import org.apache.shenyu.sdk.core.ShenyuRequest;

/**
 * ShenyuSdkRequestInterceptor.
 * For example: <br/>
 *
 * <pre>
 *   public void apply(ShenyuRequest shenyuRequest){
 *       shenyuRequest.getHeaders().put("X-Auth", Collections.singleton("currentToken"));
 *  }
 * </pre>
 *
 * <b>Configuration</b>
 *
 * <br>
 * {@code ShenyuSdkRequestInterceptor} are configured via {@link org.apache.shenyu.register.instance.api.config.RegisterConfig}. <br>
 * <pre>
 * shenyu:
 *   sdk:
 *     props:
 *       requestInterceptor:
 *         enable: true
 *         classes:
 *           - full-class-name
 *
 * </pre>
 * <br>
 * This class is similar to feign.RequestInterceptor
 */
public interface ShenyuSdkRequestInterceptor {

    /**
     * apply.
     * @param shenyuRequest shenRequest
     */
    void apply(ShenyuRequest shenyuRequest);

}

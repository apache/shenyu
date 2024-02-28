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

package org.apache.shenyu.client.core.register.extractor;

import org.apache.shenyu.client.core.register.ApiBean;
import org.springframework.context.ApplicationContext;

import java.util.List;

/**
 * Main responsibilities: Get the possible API class classes and corresponding methods,
 * and initialize and resolve them Different.<br>
 * <ul>
 *     <li>clients correspond to different implementations
 *     <li>In the Spring web scenario, collect controller
 *     <li> java EE web scenarios, collect servlet path Dubbo
 *     <li> scenarios, and collect Dubbo Service APIs
 *     <li> In other RPC scenarios, collect RPC Service APIs
 * </ul>
 */
public interface ApiBeansExtractor {
    
    /**
     * Extract apiBeans from applicationContext.
     *
     * @param applicationContext applicationContext
     * @return apiBeans
     */
    List<ApiBean> extract(ApplicationContext applicationContext);
    
}

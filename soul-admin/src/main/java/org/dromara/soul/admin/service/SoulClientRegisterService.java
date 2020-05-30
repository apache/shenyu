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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.HttpRegisterDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;

/**
 * The interface Soul client service.
 */
public interface SoulClientRegisterService {
    
    /**
     * Register http string.
     *
     * @param httpRegisterDTO the http register dto
     * @return the string
     */
    String registerHttp(HttpRegisterDTO httpRegisterDTO);
    
    /**
     * Register rpc string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    String registerRpc(MetaDataDTO metaDataDTO);
}

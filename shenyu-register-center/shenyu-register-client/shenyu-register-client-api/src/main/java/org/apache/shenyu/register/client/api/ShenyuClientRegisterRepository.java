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

package org.apache.shenyu.register.client.api;

import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.spi.SPI;

/**
 * Shenyu client register repository.
 */
@SPI
public interface ShenyuClientRegisterRepository {

    /**
     * Init.
     *
     * @param config the config
     */
    default void init(ShenyuRegisterCenterConfig config) {
    }
    
    /**
     * Persist metadata.
     *
     * @param metadata metadata
     */
    void persistInterface(MetaDataRegisterDTO metadata);
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    default void persistURI(URIRegisterDTO registerDTO) {
    }

    /**
     * persistApiDoc.
     * @param apiDocRegisterDTO apiDocRegisterDTO
     */
    default void persistApiDoc(ApiDocRegisterDTO apiDocRegisterDTO) {
    }
    
    /**
     * Close.
     */
    default void close() {
    }
}

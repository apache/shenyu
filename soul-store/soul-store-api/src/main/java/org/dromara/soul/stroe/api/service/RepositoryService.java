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

package org.dromara.soul.stroe.api.service;

import org.dromara.soul.common.extension.SPI;
import org.dromara.soul.stroe.api.dto.SelectorDTO;

/**
 * The interface Repository service.
 *
 * @author xiaoyu
 */
@SPI("jdbc")
public interface RepositoryService {

    /**
     * Save or update selector int.
     *
     * @param selectorDTO the selector dto
     * @return the int
     */
    int saveSelector(SelectorDTO selectorDTO);

}

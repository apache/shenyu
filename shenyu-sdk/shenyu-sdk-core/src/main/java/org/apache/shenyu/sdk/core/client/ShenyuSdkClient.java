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

package org.apache.shenyu.sdk.core.client;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.spi.SPI;
import org.springframework.beans.factory.ObjectProvider;

import java.io.IOException;

/**
 * ShenyuSdkClient.
 */
@SPI
public interface ShenyuSdkClient {

    /**
     * Init.
     *
     * @param shenyuConfig shenyuConfig
     * @param registerRepositoryObjectFactory registerRepositoryObjectFactory
     */
    default void init(final ShenyuConfig.RegisterConfig shenyuConfig,
                      final ObjectProvider<ShenyuInstanceRegisterRepository> registerRepositoryObjectFactory) {
    }

    /**
     * execute.
     *
     * @param request request
     * @return {@link ShenyuResponse}
     * @throws IOException error
     */
    ShenyuResponse execute(ShenyuRequest request) throws IOException;

}

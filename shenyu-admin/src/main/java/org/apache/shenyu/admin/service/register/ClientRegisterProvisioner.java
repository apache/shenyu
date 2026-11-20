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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

/**
 * Extension point invoked after shenyu admin has processed a client registration
 * through the matching {@link ShenyuClientRegisterService}.
 *
 * <p>Out-of-tree plugins (loaded through the gateway ext-plugin mechanism or
 * registered through the admin plugin management) can implement this hook to
 * provision the admin-side artifacts they require — for example the selector and
 * rule that make the gateway execute the plugin for the registered paths —
 * without patching the shenyu source code.</p>
 *
 * <p>Implementations must be idempotent, must be fast (the callback runs on the
 * register center disruptor thread) and must never throw; failures are logged
 * and isolated so that the built-in registration flow is never affected.</p>
 */
public interface ClientRegisterProvisioner {

    /**
     * The plugin name this provisioner belongs to, used for logging and diagnostics.
     *
     * @return plugin name
     */
    String pluginName();

    /**
     * Whether this provisioner wants to handle the given registration.
     *
     * @param metaDataRegisterDTO meta data register dto.
     * @return true to invoke {@link #provision(MetaDataRegisterDTO)}
     */
    default boolean supports(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return true;
    }

    /**
     * Provision admin-side resources for the given registration.
     *
     * @param metaDataRegisterDTO meta data register dto.
     */
    void provision(MetaDataRegisterDTO metaDataRegisterDTO);
}

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
 * Extension point invoked for shenyu admin client metadata registrations.
 *
 * <p>Out-of-tree plugins can implement this hook to provision the admin-side
 * artifacts they require, for example the selector and rule that make the
 * gateway execute the plugin for the registered paths. When a matching
 * {@link ShenyuClientRegisterService} exists, the hook runs after that service
 * returns successfully. Otherwise, the hook still runs so a plugin can handle
 * its own rpcType without patching shenyu.</p>
 *
 * <p>The callback runs on the register center disruptor thread. Implementations
 * must therefore be idempotent, thread-safe and fast. Failures are logged and
 * isolated so that built-in registration and the remaining provisioners are
 * never affected. See {@code docs/client-register-provisioner.md} for the
 * supported bean registration paths.</p>
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

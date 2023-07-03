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

package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.NotImplementedException;

public class DiscoveryProcessorHolder {

    private final DiscoveryProcessor defaultDiscoveryProcessor;

    private final DiscoveryProcessor localDiscoveryProcessor;

    public DiscoveryProcessorHolder(final DiscoveryProcessor defaultDiscoveryProcessor, final DiscoveryProcessor localDiscoveryProcessor) {
        this.defaultDiscoveryProcessor = defaultDiscoveryProcessor;
        this.localDiscoveryProcessor = localDiscoveryProcessor;
    }

    /**
     * chooseProcessor.
     *
     * @param mode mode
     * @return DiscoveryProcessor
     */
    public DiscoveryProcessor chooseProcessor(final String mode) {
        if (DiscoveryMode.LOCAL.name().equalsIgnoreCase(mode)) {
            return localDiscoveryProcessor;
        } else if (DiscoveryMode.ZOOKEEPER.name().equalsIgnoreCase(mode)) {
            return defaultDiscoveryProcessor;
        } else {
            throw new NotImplementedException("shenyu discovery mode current didn't support " + mode);
        }
    }

}

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

package org.dromara.soul.limiter.redis.client.lettuce;

import java.util.List;

/**
 * The interface Lettuce client.
 *
 * @author xiaoyu
 */
public interface LettuceTypeClient {

    /**
     * Evalsha object.
     *
     * @param script the script
     * @param keys   the keys
     * @param args   the args
     * @return the object
     */
    Object evalsha(final String script, final List<String> keys, final List<String> args);
}

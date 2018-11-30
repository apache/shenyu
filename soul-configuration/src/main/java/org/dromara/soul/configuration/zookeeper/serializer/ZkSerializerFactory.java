/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.configuration.zookeeper.serializer;

import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.common.utils.SpiLoadFactory;

import java.util.Objects;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

/**
 * ZkSerializer Factory.
 *
 * @author xiaoyu(Myth)
 */
public class ZkSerializerFactory {

    private static final ServiceLoader<ZkSerializer> SERVICE_LOADER = SpiLoadFactory.loadAll(ZkSerializer.class);

    /**
     * product  ZkSerializer with className.
     *
     * @param className className
     * @return ZkSerializer zk serializer
     */
    public static ZkSerializer of(final String className) {
        return StreamSupport.stream(SERVICE_LOADER.spliterator(), false)
                .filter(service ->
                        Objects.equals(service.getClass().getName().substring(service.getClass().getName().lastIndexOf(".") + 1,
                                service.getClass().getName().length()),
                                className)).findFirst().orElse(new KryoSerializer());
    }
}

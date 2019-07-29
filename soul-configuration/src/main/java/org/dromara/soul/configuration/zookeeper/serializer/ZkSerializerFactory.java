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
import org.dromara.soul.common.enums.SerializeEnum;

/**
 * ZkSerializer Factory.
 *
 * @author xiaoyu(Myth)
 */
public class ZkSerializerFactory {


    /**
     * Of zk serializer.
     *
     * @param name the name
     * @return the zk serializer
     */
    public static ZkSerializer of(final String name) {
        final SerializeEnum serializeEnum = SerializeEnum.acquire(name);
        switch (serializeEnum) {
            case KRYO:
                return new KryoSerializer();
            case JDK:
                return new JavaSerializer();
            case HESSIAN:
                return new HessianSerializer();
            case PROTOSTUFF:
                return new ProtostuffSerializer();
            default:
                return new JavaSerializer();
        }
    }
}

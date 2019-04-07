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

import com.alibaba.fastjson.JSON;
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.springframework.data.redis.serializer.SerializationException;

import java.nio.charset.Charset;

/**
 * The type FastJson serializer.
 *
 * @author xiaoyu
 */
public class FastJsonSerializer implements ZkSerializer {

    @Override
    public byte[] serialize(final Object o) throws SerializationException {
        if (o == null) {
            return new byte[]{};
        }
        if (o instanceof String) {
            return o.toString().getBytes(Charset.forName("UTF-8"));
        }
        return JSON.toJSONString(o).getBytes(Charset.forName("UTF-8"));
    }

    @Override
    public Object deserialize(final byte[] bytes) throws SerializationException {
        if (bytes == null) {
            return null;
        }
        return new String(bytes, Charset.forName("UTF-8"));
    }
}

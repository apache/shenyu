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

import com.dyuproject.protostuff.LinkedBuffer;
import com.dyuproject.protostuff.ProtostuffIOUtil;
import com.dyuproject.protostuff.Schema;
import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.common.exception.SerializerException;
import org.objenesis.Objenesis;
import org.objenesis.ObjenesisStd;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * ProtostuffSerializer.
 *
 * @author xiaoyu
 */
@SuppressWarnings("unchecked")
public class ProtostuffSerializer implements ZkSerializer {

    private static final SchemaCache CACHED_SCHEMA = SchemaCache.getInstance();

    private static final Objenesis OBJENESIS = new ObjenesisStd(true);

    private static <T> Schema<T> getSchema(final Class<T> cls) {
        return (Schema<T>) CACHED_SCHEMA.get(cls);
    }

    @Override
    public byte[] serialize(final Object obj) {
        Class cls = obj.getClass();
        LinkedBuffer buffer = LinkedBuffer.allocate(LinkedBuffer.DEFAULT_BUFFER_SIZE);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        try {
            Schema schema = getSchema(cls);
            ProtostuffIOUtil.writeTo(outputStream, obj, schema, buffer);
        } catch (IOException e) {
            throw new SerializerException(e.getMessage(), e);
        } finally {
            buffer.clear();
        }
        return outputStream.toByteArray();
    }

    @Override
    public Object deserialize(final byte[] bytes) throws ZkMarshallingError {
        Object object;
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(bytes);
            Class cls = Object.class;
            object = OBJENESIS.newInstance(Object.class);
            Schema schema = getSchema(cls);
            ProtostuffIOUtil.mergeFrom(inputStream, object, schema);
            return object;
        } catch (IOException e) {
            throw new SerializerException(e.getMessage(), e);
        }
    }

}


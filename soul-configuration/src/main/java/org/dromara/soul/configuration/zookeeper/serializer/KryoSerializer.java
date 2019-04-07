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

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.common.exception.SerializerException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


/**
 * KryoSerializer.
 *
 * @author xiaoyu
 */
public class KryoSerializer implements ZkSerializer {

    @Override
    public byte[] serialize(final Object obj) {
        byte[] bytes;
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            //获取kryo对象
            Kryo kryo = new Kryo();
            Output output = new Output(outputStream);
            kryo.writeClassAndObject(output, obj);
            bytes = output.toBytes();
            output.flush();
        } catch (IOException ex) {
            throw new SerializerException("kryo serialize error" + ex.getMessage());
        }
        return bytes;
    }

    @Override
    public Object deserialize(final byte[] bytes) throws ZkMarshallingError {
        Object object;
        try (ByteArrayInputStream inputStream = new ByteArrayInputStream(bytes)) {
            Kryo kryo = new Kryo();
            Input input = new Input(inputStream);
            object = kryo.readClassAndObject(input);
            input.close();
        } catch (IOException e) {
            throw new SerializerException("kryo deSerialize error" + e.getMessage());
        }
        return object;
    }

}

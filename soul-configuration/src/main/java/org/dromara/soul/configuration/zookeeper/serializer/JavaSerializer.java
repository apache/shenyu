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

import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.common.exception.SerializerException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

/**
 * JavaSerializer.
 *
 * @author xiaoyu
 */
public class JavaSerializer implements ZkSerializer {

    @Override
    public byte[] serialize(final Object obj) {
        try (ByteArrayOutputStream arrayOutputStream = new ByteArrayOutputStream(); ObjectOutput objectOutput = new ObjectOutputStream(arrayOutputStream)) {
            objectOutput.writeObject(obj);
            objectOutput.flush();
            return arrayOutputStream.toByteArray();
        } catch (IOException e) {
            throw new SerializerException("JAVA serialize error " + e.getMessage());
        }
    }

    @Override
    public Object deserialize(final byte[] bytes) throws ZkMarshallingError {
        try (ByteArrayInputStream arrayInputStream = new ByteArrayInputStream(bytes); ObjectInput input = new ObjectInputStream(arrayInputStream)) {
            return input.readObject();
        } catch (IOException | ClassNotFoundException e) {
            throw new SerializerException("JAVA deSerialize error " + e.getMessage());
        }
    }

}

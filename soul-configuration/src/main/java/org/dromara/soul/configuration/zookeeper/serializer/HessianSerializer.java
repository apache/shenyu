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

import com.caucho.hessian.io.Hessian2Input;
import com.caucho.hessian.io.Hessian2Output;
import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.common.exception.SerializerException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


/**
 * HessianSerializer.
 *
 * @author xiaoyu
 */
public class HessianSerializer implements ZkSerializer {

    @Override
    public byte[] serialize(final Object obj) {
        Hessian2Output hos;
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            hos = new Hessian2Output(bos);
            hos.writeObject(obj);
            hos.flush();
            return bos.toByteArray();
        } catch (IOException ex) {
            throw new SerializerException("Hessian serialize error " + ex.getMessage());
        }

    }

    @Override
    public Object deserialize(final byte[] bytes) throws ZkMarshallingError {
        ByteArrayInputStream bios;
        try {
            bios = new ByteArrayInputStream(bytes);
            Hessian2Input his = new Hessian2Input(bios);
            return his.readObject();
        } catch (IOException e) {
            throw new SerializerException("Hessian deSerialize error " + e.getMessage());
        }
    }
}

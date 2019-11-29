package org.dromara.soul.extend.demo.serializer;

import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;

/**
 * @author xiaoyu(Myth)
 */
public class CustomSerializer implements ZkSerializer {

    @Override
    public byte[] serialize(final Object data) throws ZkMarshallingError {
        return new byte[0];
    }

    @Override
    public Object deserialize(final byte[] bytes) throws ZkMarshallingError {
        System.out.println("自定义zk序列化方式");
        return null;
    }
}

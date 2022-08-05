package org.apache.shenyu.client.tars;

import org.apache.shenyu.client.tars.common.dto.TarsRpcExt;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

/**
 * Test case for {@link TarsRpcExt}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class TarsRpcExtTest {

    @Test
    public void testTarsRpcExt() {
        List<TarsRpcExt.RpcExt> rpcExtList = new ArrayList<>();
        TarsRpcExt.RpcExt rpcExt = new TarsRpcExt.RpcExt();
        rpcExt.setMethodName("methodName");
        rpcExt.setParams(null);
        rpcExt.setReturnType("returnType");
        rpcExtList.add(rpcExt);
        TarsRpcExt tarsRpcExt = new TarsRpcExt();
        tarsRpcExt.setMethodInfo(rpcExtList);
        Assertions.assertNotNull(tarsRpcExt.toString());
        Assertions.assertNotNull(tarsRpcExt.getMethodInfo());
        Assertions.assertNotNull(rpcExt.toString());
        Assertions.assertNotNull(rpcExt.getMethodName());
        Assertions.assertNotNull(rpcExt.getReturnType());
        Assertions.assertNull(rpcExt.getParams());
    }
}


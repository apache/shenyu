package org.dromara.soul.web.dubbo;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

@RunWith(MockitoJUnitRunner.class)
public class DefaultDubboParamResolveServiceTest {

    @InjectMocks
    private DefaultDubboParamResolveService impl;

    @Test
    public void testBuildParameterWithNull() {
        String body = "{\"id\":null,\"name\":null}";
        String parameterTypes = "org.dromara.soul.web.dubbo.DubboMultiParameterResolveServiceImplTest.Student";
        Pair<String[], Object[]> pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));
        Map map = (Map) pair.getRight()[0];
        assertNull(map.get("id"));
        assertNull(map.get("name"));

        body = "{\"dubboTest\":{\"id\":null,\"name\":null},\"idLists\":[null,null],\"idMaps\":{\"id2\":null,\"id1\":null}}";
        parameterTypes = "org.dromara.soul.web.dubbo.DubboMultiParameterResolveServiceImplTest.ComplexBean";
        pair = impl.buildParameter(body, parameterTypes);
        assertThat(pair.getLeft().length, is(1));
        assertThat(pair.getRight().length, is(1));
        map = (Map) pair.getRight()[0];
        Map dubboTest = (Map) map.get("dubboTest");
        assertNull(dubboTest.get("id"));
        assertNull(dubboTest.get("name"));
        List idList = (List) map.get("idLists");
        assertNull(idList.get(0));
        assertNull(idList.get(1));
    }
}

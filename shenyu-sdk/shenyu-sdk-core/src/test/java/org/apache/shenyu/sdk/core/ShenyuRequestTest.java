package org.apache.shenyu.sdk.core;


import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Test for {@link ShenyuRequest}.
 */
public class ShenyuRequestTest {

    @Test
    public void testShenyuRequest() {
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("header1", "header2"));
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);
        
        Assert.assertNotNull(request);
    }

}

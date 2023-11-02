package org.apache.shenyu.sdk.core;


import org.apache.http.HttpStatus;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Test for {@link ShenyuResponse}.
 */
public class ShenyuResponseTest {

    @Test
    public void testShenyuResponse() {
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("header1", "header2"));
        String body = "{key1:\"value1\"}";
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);

        ShenyuResponse response = new ShenyuResponse(
                HttpStatus.SC_OK,
                "success",
                headerMap,
                body,
                request
        );
        Assert.assertNotNull(response);
    }
}

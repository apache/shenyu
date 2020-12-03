package org.dromara.soul.sync.data.http.support;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Mock Http Fetch Config
 *
 * @author David Liu
 */
public class MockConfigFetchHandler extends AbstractHandler {
    @Override
    public void handle(String s, Request request, HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws IOException {
        // Declare response encoding and types
        httpServletResponse.setContentType("application/json; charset=utf-8");
        
        // Declare response status code
        httpServletResponse.setStatus(HttpServletResponse.SC_OK);
        
        httpServletResponse.getWriter().println(this.mockResponseJson());
        
        // Inform jetty that this request has now been handled
        request.setHandled(true);
        
    }
    
    // mock response
    private String mockResponseJson() {
        return "{\"code\":200,\"message\":\"success\",\"data\":{\"META_DATA\":{\"md5\":\"d751713988987e9331980363e24189ce\",\"lastModifyTime\":1606982496942,\"data\":[]}," +
                "\"SELECTOR\":{\"md5\":\"d751713988987e9331980363e24189ce\",\"lastModifyTime\":1606982496931,\"data\":[]},\"PLUGIN\":{\"md5\":\"ff9f3045505109e66c403fcc6a7a9a12\",\"lastModifyTime\":1606982496917,\"data\":[{\"id\":\"1\",\"name\":\"sign\",\"config\":null,\"role\":0,\"enabled\":false},{\"id\":\"2\",\"name\":\"waf\",\"config\":\"{\\\"model\\\":\\\"black\\\"}\",\"role\":0,\"enabled\":false},{\"id\":\"3\",\"name\":\"rewrite\",\"config\":null,\"role\":0,\"enabled\":false},{\"id\":\"4\",\"name\":\"rate_limiter\",\"config\":\"{\\\"master\\\":\\\"mymaster\\\",\\\"mode\\\":\\\"Standalone\\\",\\\"url\\\":\\\"192.168.1.1:6379\\\",\\\"password\\\":\\\"abc\\\"}\",\"role\":0,\"enabled\":false},{\"id\":\"5\",\"name\":\"divide\",\"config\":null,\"role\":0,\"enabled\":true},{\"id\":\"6\",\"name\":\"dubbo\",\"config\":\"{\\\"register\\\":\\\"zookeeper://localhost:2181\\\"}\",\"role\":0,\"enabled\":false},{\"id\":\"7\",\"name\":\"monitor\",\"config\":\"{\\\"metricsName\\\":\\\"prometheus\\\",\\\"host\\\":\\\"localhost\\\",\\\"port\\\":\\\"9190\\\",\\\"async\\\":\\\"true\\\"}\",\"role\":0,\"enabled\":false},{\"id\":\"8\",\"name\":\"springCloud\",\"config\":null,\"role\":0,\"enabled\":false},{\"id\":\"9\",\"name\":\"hystrix\",\"config\":null,\"role\":0,\"enabled\":false}]},\"APP_AUTH\":{\"md5\":\"d751713988987e9331980363e24189ce\",\"lastModifyTime\":1606982496898,\"data\":[]},\"RULE\":{\"md5\":\"d751713988987e9331980363e24189ce\",\"lastModifyTime\":1606982496924,\"data\":[]}}}";
    }
}

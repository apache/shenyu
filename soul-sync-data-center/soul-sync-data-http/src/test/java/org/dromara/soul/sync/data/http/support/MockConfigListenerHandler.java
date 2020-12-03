package org.dromara.soul.sync.data.http.support;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Mock Http Config Listener
 *
 * @author David Liu
 */
public class MockConfigListenerHandler extends AbstractHandler {
    @Override
    public void handle(String s, Request request, HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws IOException, ServletException {
        // Declare response encoding and types
        httpServletResponse.setContentType("application/json; charset=utf-8");
        
        // Declare response status code
        httpServletResponse.setStatus(HttpServletResponse.SC_OK);
        
        httpServletResponse.getWriter().println(this.mockResponseJson());
        
        // Inform jetty that this request has now been handled
        request.setHandled(true);
    }
    
    // mock plugin config has changed
    private String mockResponseJson() {
        return "{\"code\":200,\"message\":\"success\",\"data\":[\"PLUGIN\"]}";
    }
}

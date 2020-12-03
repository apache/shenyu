/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

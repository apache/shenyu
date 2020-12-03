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

import lombok.extern.slf4j.Slf4j;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Mock Http Fetch Config.
 *
 * @author David Liu
 */
@Slf4j
public class MockConfigFetchHandler extends AbstractHandler {
    @Override
    public void handle(final String s, final Request request, final HttpServletRequest httpServletRequest, final HttpServletResponse httpServletResponse) throws IOException {
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
        try (FileInputStream fis = new FileInputStream(this.getClass().getClassLoader().getResource("mock_configs_listen_response.json").getPath());
             InputStreamReader reader = new InputStreamReader(fis);
             BufferedReader bufferedReader = new BufferedReader(reader);
        ) {
            StringBuilder builder = new StringBuilder();
            bufferedReader.lines().forEach(builder::append);
            return builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
}

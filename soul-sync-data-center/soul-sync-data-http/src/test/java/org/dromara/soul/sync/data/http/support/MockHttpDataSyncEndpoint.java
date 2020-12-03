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

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ContextHandler;
import org.eclipse.jetty.server.handler.ContextHandlerCollection;

/**
 * mock Http Data Sync Endpoint
 *
 * @author David Liu
 */
public class MockHttpDataSyncEndpoint {
    private final int port;
    private Server server;
    
    public MockHttpDataSyncEndpoint(int port) {
        this.port = port;
    }
    
    public void start() throws Exception {
        server = new Server(port);
        
        // mock configs fetch api
        ContextHandler configsFetchCtx = new ContextHandler("/configs/fetch");
        configsFetchCtx.setHandler(new MockConfigFetchHandler());
        
        // mock configs polling api
        ContextHandler configsListenerCtx = new ContextHandler("/configs/listener");
        configsListenerCtx.setHandler(new MockConfigListenerHandler());
        
        ContextHandlerCollection contexts = new ContextHandlerCollection();
        contexts.addHandler(configsFetchCtx);
        contexts.addHandler(configsListenerCtx);
        
        server.setHandler(contexts);
        
        server.start();
    }
    
    public void stop() throws Exception {
        server.stop();
    }
}

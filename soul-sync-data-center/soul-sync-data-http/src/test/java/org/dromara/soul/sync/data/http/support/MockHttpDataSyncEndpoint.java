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

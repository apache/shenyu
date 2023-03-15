package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.register.client.server.apollo.ApolloClient;

public class ApolloDataChangedListener2 extends AbstractNodeDataChangedListener {
    private final ApolloClient apolloClient;

    public ApolloDataChangedListener2(ApolloClient apolloClient) {
        this.apolloClient = apolloClient;
    }

    @Override
    public void createOrUpdate(String pluginPath, Object data) {
        this.apolloClient.createItem(pluginPath, data, "");
    }

    @Override
    public void deleteNode(String pluginPath) {

    }

    @Override
    public void deletePathRecursive(String selectorParentPath) {

    }
}

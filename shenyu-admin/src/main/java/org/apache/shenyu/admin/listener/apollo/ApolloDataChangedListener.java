package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.register.client.server.apollo.ApolloClient;

public class ApolloDataChangedListener extends AbstractNodeDataChangedListener {
    private final ApolloClient apolloClient;

    public ApolloDataChangedListener(ApolloClient apolloClient) {
        this.apolloClient = apolloClient;
    }

    @Override
    public void createOrUpdate(String pluginPath, Object data) {
        this.apolloClient.createOrUpdateItem(pluginPath, data, "");
        this.apolloClient.publishNamespace("create or update node data", "");
    }

    @Override
    public void deleteNode(String pluginPath) {
        this.apolloClient.removeItem(pluginPath);
    }

    @Override
    public void deletePathRecursive(String selectorParentPath) {
        this.apolloClient.removeItem(selectorParentPath);
    }
}

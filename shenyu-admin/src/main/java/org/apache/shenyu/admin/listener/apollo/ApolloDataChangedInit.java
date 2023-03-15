package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.register.client.server.apollo.ApolloClient;

import java.util.Objects;
import java.util.stream.Stream;

public class ApolloDataChangedInit extends AbstractDataChangedInit {
    private final ApolloClient apolloClient;

    public ApolloDataChangedInit(ApolloClient apolloClient) {
        this.apolloClient = apolloClient;
    }

    @Override
    protected boolean notExist() {
        return Stream.of(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.META_DATA_ID).allMatch(
            this::dataIdNotExist);
    }

    private boolean dataIdNotExist(final String pluginDataId) {
        return Objects.isNull(apolloClient.getItemValue(pluginDataId));
    }
}

package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.listener.AbstractListDataChangedListener;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.register.client.server.apollo.ApolloClient;
import org.springframework.util.StringUtils;

public class ApolloDataChangedListener extends AbstractListDataChangedListener {
    private final ApolloClient apolloClient;

    public ApolloDataChangedListener(final ApolloClient apolloClient) {
        super(new ChangeData(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.SELECTOR_DATA_ID,
                NacosPathConstants.RULE_DATA_ID, NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.META_DATA_ID));
        this.apolloClient = apolloClient;
    }

    @Override
    public void publishConfig(String dataId, Object data) {
        this.apolloClient.createItem(dataId, data, "");
        this.apolloClient.publishNamespace("publish config: " + dataId, "");
    }

    @Override
    public String getConfig(String dataId) {
        String config = this.apolloClient.getItemValue(dataId);
        return StringUtils.hasLength(config) ? config : NacosPathConstants.EMPTY_CONFIG_DEFAULT_VALUE;
    }
}

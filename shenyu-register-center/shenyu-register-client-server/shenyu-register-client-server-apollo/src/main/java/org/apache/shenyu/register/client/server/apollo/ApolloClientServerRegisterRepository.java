package org.apache.shenyu.register.client.server.apollo;

import com.ctrip.framework.apollo.core.ConfigConsts;
import com.ctrip.framework.apollo.spring.config.PropertySourcesConstants;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.spi.Join;

import java.util.Properties;

@Join
public class ApolloClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private ShenyuClientServerRegisterPublisher publisher;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher,
                     final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;

        Properties properties = config.getProps();
        Properties apolloProperties = new Properties();
        apolloProperties.setProperty("app.id", properties.getProperty("appId"));
        apolloProperties.setProperty(ConfigConsts.APOLLO_META_KEY, properties.getProperty("meta"));
        apolloProperties.setProperty(ConfigConsts.APOLLO_CLUSTER_KEY, properties.getProperty("cluster", ConfigConsts.CLUSTER_NAME_DEFAULT));
        apolloProperties.setProperty(PropertySourcesConstants.APOLLO_BOOTSTRAP_NAMESPACES, properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION));
        System.setProperties(apolloProperties);
    }

    private void initSubscribe() {

    }

    @Override
    public void close() {
        this.publisher.close();
    }


}

package org.apache.shenyu.admin.listener.consul;

import com.ecwid.consul.v1.ConsulClient;
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.ConsulConstants;

import java.util.Objects;

/**
 * The type Consul data changed init.
 */
public class ConsulDataChangedInit extends AbstractDataChangedInit {

    private final ConsulClient consulClient;

    /**
     * Instantiates a new Consul data changed init.
     *
     * @param consulClient the Consul client
     */
    public ConsulDataChangedInit(final ConsulClient consulClient) {
        this.consulClient = consulClient;
    }

    @Override
    protected boolean notExist() {
        return dataKeyNotExist(ConsulConstants.PLUGIN_DATA)
                && dataKeyNotExist(ConsulConstants.AUTH_DATA)
                && dataKeyNotExist(ConsulConstants.META_DATA);
    }

    private boolean dataKeyNotExist(final String dataKey) {
        return Objects.isNull(consulClient.getKVValue(dataKey).getValue());
    }
}

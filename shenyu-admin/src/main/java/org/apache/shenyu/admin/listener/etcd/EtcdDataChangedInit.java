package org.apache.shenyu.admin.listener.etcd;

import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.DefaultPathConstants;

/**
 * The type Etcd data changed init.
 */
public class EtcdDataChangedInit extends AbstractDataChangedInit {

    private final EtcdClient etcdClient;

    /**
     * Instantiates a new Etcd data changed init.
     *
     * @param etcdClient the etcdClient client
     */
    public EtcdDataChangedInit(final EtcdClient etcdClient) {
        this.etcdClient = etcdClient;
    }

    @Override
    protected boolean notExist() {
        return !etcdClient.exists(DefaultPathConstants.PLUGIN_PARENT)
                && !etcdClient.exists(DefaultPathConstants.APP_AUTH_PARENT)
                && !etcdClient.exists(DefaultPathConstants.META_DATA);
    }
}

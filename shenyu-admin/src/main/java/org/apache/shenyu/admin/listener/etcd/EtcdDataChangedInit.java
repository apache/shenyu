package org.apache.shenyu.admin.listener.etcd;

import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * TODO
 *
 * @author KevinClair
 **/
public class EtcdDataChangedInit extends AbstractDataChangedInit {

    private static final Logger LOG = LoggerFactory.getLogger(EtcdDataChangedInit.class);

    private final EtcdClient etcdClient;

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

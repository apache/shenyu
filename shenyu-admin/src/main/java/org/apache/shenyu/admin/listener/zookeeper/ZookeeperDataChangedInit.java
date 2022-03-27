package org.apache.shenyu.admin.listener.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.DefaultPathConstants;

/**
 * TODO
 */
public class ZookeeperDataChangedInit extends AbstractDataChangedInit {

    private final ZkClient zkClient;

    public ZookeeperDataChangedInit(final ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    protected boolean notExist() {
        return !zkClient.exists(DefaultPathConstants.PLUGIN_PARENT)
                && !zkClient.exists(DefaultPathConstants.APP_AUTH_PARENT)
                && !zkClient.exists(DefaultPathConstants.META_DATA);
    }
}

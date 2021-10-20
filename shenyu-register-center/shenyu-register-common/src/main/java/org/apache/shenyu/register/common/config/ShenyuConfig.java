package org.apache.shenyu.register.common.config;

import org.apache.shenyu.register.common.constant.InstanceConstants;

/**
 * ShenyuConfig.
 */
public class ShenyuConfig {

    private String instanceName = InstanceConstants.DEFAULT_INSTANCE_NAME;

    /**
     * Gets instance name.
     *
     * @return the instance name
     */
    public String getInstanceName() {
        return instanceName;
    }

    /**
     * Sets instance name.
     *
     * @param instanceName the instance name
     */
    public void setInstanceName(final String instanceName) {
        this.instanceName = instanceName;
        InstanceConstants.setInstanceName(instanceName);
    }
}

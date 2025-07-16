package org.apache.shenyu.register.common.dto;

public class InstanceBeatInfoDTO {
    /**
     * instance ip.
     */
    private String instanceIp;

    /**
     * instance port.
     */
    private String instancePort;

    /**
     * instance type.
     */
    private String instanceType;

    /**
     * instance info.
     */
    private String instanceInfo;

    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * get instanceIp.
     *
     * @return instanceIp
     */
    public String getInstanceIp() {
        return instanceIp;
    }

    /**
     * set instanceIp.
     *
     * @param instanceIp instanceIp
     */
    public void setInstanceIp(final String instanceIp) {
        this.instanceIp = instanceIp;
    }

    /**
     * get instancePort.
     *
     * @return instancePort
     */
    public String getInstancePort() {
        return instancePort;
    }

    /**
     * set instancePort.
     *
     * @param instancePort instancePort
     */
    public void setInstancePort(final String instancePort) {
        this.instancePort = instancePort;
    }

    /**
     * get instanceType.
     *
     * @return instanceType
     */
    public String getInstanceType() {
        return instanceType;
    }

    /**
     * set instanceType.
     *
     * @param instanceType instanceType
     */
    public void setInstanceType(final String instanceType) {
        this.instanceType = instanceType;
    }

    /**
     * get instanceInfo.
     *
     * @return instanceInfo
     */
    public String getInstanceInfo() {
        return instanceInfo;
    }

    /**
     * set instanceInfo.
     *
     * @param instanceInfo instanceInfo
     */
    public void setInstanceInfo(final String instanceInfo) {
        this.instanceInfo = instanceInfo;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

}

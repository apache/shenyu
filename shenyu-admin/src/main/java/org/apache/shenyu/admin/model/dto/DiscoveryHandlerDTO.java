package org.apache.shenyu.admin.model.dto;

public class DiscoveryHandlerDTO {

    private String id;

    private String discoveryId;

    private String handler;

    private String listenerNode;

    private String props;

    /**
     * getId.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * getDiscoveryId.
     *
     * @return discoveryId
     */
    public String getDiscoveryId() {
        return discoveryId;
    }

    /**
     * setDiscoveryId.
     *
     * @param discoveryId discoveryId
     */
    public void setDiscoveryId(final String discoveryId) {
        this.discoveryId = discoveryId;
    }

    /**
     * getHandler.
     *
     * @return handler
     */
    public String getHandler() {
        return handler;
    }

    /**
     * setHandler.
     *
     * @param handler handler
     */
    public void setHandler(String handler) {
        this.handler = handler;
    }

    /**
     * getListenerNode.
     *
     * @return listenerNode
     */
    public String getListenerNode() {
        return listenerNode;
    }

    /**
     * setListenerNode.
     *
     * @param listenerNode listenerNode
     */
    public void setListenerNode(final String listenerNode) {
        this.listenerNode = listenerNode;
    }

    /**
     * getProps.
     *
     * @return props
     */
    public String getProps() {
        return props;
    }

    /**
     * setProps.
     *
     * @param props props
     */
    public void setProps(String props) {
        this.props = props;
    }
}

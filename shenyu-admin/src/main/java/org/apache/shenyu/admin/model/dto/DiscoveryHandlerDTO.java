package org.apache.shenyu.admin.model.dto;

public class DiscoveryHandlerDTO {

    private String id;

    private String discoveryId;

    private String handler;

    private String listenerNode;

    private String props;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getDiscoveryId() {
        return discoveryId;
    }

    public void setDiscoveryId(String discoveryId) {
        this.discoveryId = discoveryId;
    }

    public String getHandler() {
        return handler;
    }

    public void setHandler(String handler) {
        this.handler = handler;
    }

    public String getListenerNode() {
        return listenerNode;
    }

    public void setListenerNode(String listenerNode) {
        this.listenerNode = listenerNode;
    }

    public String getProps() {
        return props;
    }

    public void setProps(String props) {
        this.props = props;
    }
}

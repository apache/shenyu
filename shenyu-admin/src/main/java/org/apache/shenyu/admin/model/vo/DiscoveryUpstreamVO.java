package org.apache.shenyu.admin.model.vo;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

public class DiscoveryUpstreamVO {

    private String id;

    private String discoveryHandlerId;

    /**
     * protocol.
     */
    private String protocol;

    /**
     * url.
     */
    private String url;

    /**
     * status.
     */
    private Integer status;

    /**
     * weight.
     */
    private Integer weight;

    /**
     * props.
     */
    private String props;


    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }


    public String getDiscoveryHandlerId() {
        return discoveryHandlerId;
    }

    public void setDiscoveryHandlerId(String discoveryHandlerId) {
        this.discoveryHandlerId = discoveryHandlerId;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }

    public Integer getWeight() {
        return weight;
    }

    public void setWeight(Integer weight) {
        this.weight = weight;
    }

    public String getProps() {
        return props;
    }

    public void setProps(String props) {
        this.props = props;
    }
}

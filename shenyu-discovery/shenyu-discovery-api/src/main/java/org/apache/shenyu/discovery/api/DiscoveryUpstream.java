package org.apache.shenyu.discovery.api;

public class DiscoveryUpstream {
    private String protocol;

    private String url;

    private Integer status;

    private Integer weight;

    private String props;

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(final String url) {
        this.url = url;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(final Integer status) {
        this.status = status;
    }

    public Integer getWeight() {
        return weight;
    }

    public void setWeight(final Integer weight) {
        this.weight = weight;
    }

    public String getProps() {
        return props;
    }

    public void setProps(final String props) {
        this.props = props;
    }
}

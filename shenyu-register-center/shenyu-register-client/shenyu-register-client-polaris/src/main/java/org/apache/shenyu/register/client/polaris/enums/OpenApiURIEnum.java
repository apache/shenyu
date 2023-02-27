package org.apache.shenyu.register.client.polaris.enums;

import org.apache.hc.core5.http.Method;

public enum OpenApiURIEnum {

    /**
     * release config file
     */
    URI_RELEASE_CONFIG("/config/v1/configfiles/release",Method.POST),

    /**
     * create config file
     */
    URI_CREATE_CONFIG("/config/v1/configfiles",Method.POST),


    /**
     * update config file
     */
    URI_UPDATE_CONFIG("/config/v1/configfiles",Method.PUT),

    /**
     * get config file
     */
    URI_GET_CONFIG("/config/v1/configfiles",Method.GET);

    private String uri;
    private Method method;

    OpenApiURIEnum(String uri, Method method) {
        this.uri = uri;
        this.method = method;
    }

    public String getUri() {
        return uri;
    }

    public Method getMethod() {
        return method;
    }
}

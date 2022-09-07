package org.apache.shenyu.sdk.starter.core.factory;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;

public interface RequestPostProcessor {

    /**
     * order.
     *
     * @return {@link int}
     */
    default int order() {
        return Integer.MAX_VALUE;
    }

    /**
     * postProcessor.
     *
     * @param request request
     * @param args args
     * @return {@link RequestTemplate}
     */
    RequestTemplate postProcessor(RequestTemplate request, Object[] args);

}
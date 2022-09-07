package org.apache.shenyu.sdk.starter.core.processor;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.ShenyuRequest;
import org.apache.shenyu.sdk.starter.core.factory.RequestPostProcessor;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.lang.annotation.Annotation;
import java.util.List;

public class ParamPostProcessor implements RequestPostProcessor {

    @Override
    public int order() {
        return Integer.MIN_VALUE;
    }

    @Override
    public RequestTemplate postProcessor(RequestTemplate request, Object[] args) {
        final List<RequestTemplate.ParamMetadata> paramMetadataList = request.getParamMetadataList();
        if (!CollectionUtils.isEmpty(paramMetadataList)) {
            request.setUrl(urlJoinParams(request.getUrl(), paramMetadataList, args));
        }

        if (request.getHttpMethod() == ShenyuRequest.HttpMethod.POST) {
            // TODO JsonUtils
//            request.setBody(ShenyuRequest.Body.create(getRequestBody(paramMetadataList, args)));
        }
        return request;
    }

    private String urlJoinParams(String url, List<RequestTemplate.ParamMetadata> paramMetadataList, Object[] args) {
        boolean isFistParam = !url.contains("=");
        for (RequestTemplate.ParamMetadata paramMetadata : paramMetadataList) {
            Annotation[] paramAnnotations = paramMetadata.getParamAnnotations();
            if (paramAnnotations != null && paramAnnotations.length > 0) {
                for (Annotation anno : paramAnnotations) {
                    if (anno instanceof RequestParam) {
                        if (!url.startsWith("?")) {
                            url = url.concat("?");
                        }
                        if (!isFistParam) {
                            url = url.concat("&");
                        }
                        isFistParam = false;
                        url = url.concat(((RequestParam) anno).value());
                        url = url.concat("=");
                        url = url.concat(String.valueOf(args[paramMetadata.getParamIndexOnMethod()]));
                    }
                }
            }
        }
        return url;
    }

    private Object getRequestBody(List<RequestTemplate.ParamMetadata> paramMetadataList, Object[] args) {
        for (RequestTemplate.ParamMetadata paramMetadata : paramMetadataList) {
            Annotation[] paramAnnotations = paramMetadata.getParamAnnotations();
            if (paramAnnotations != null && paramAnnotations.length > 0) {
                for (Annotation anno : paramAnnotations) {
                    if (anno instanceof RequestBody) {
                        return args[paramMetadata.getParamIndexOnMethod()];
                    }
                }
            }
        }
        return null;
    }

}

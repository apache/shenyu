package org.apache.shenyu.sdk.starter.core.support;

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
    public RequestTemplate postProcessor(final RequestTemplate request, final Object[] args) {
        final List<RequestTemplate.ParamMetadata> paramMetadataList = request.getParamMetadataList();
        if (!CollectionUtils.isEmpty(paramMetadataList)) {
            request.setUrl(urlJoinParams(request.getUrl(), paramMetadataList, args));
        }
        if (request.getHttpMethod() == ShenyuRequest.HttpMethod.POST) {
//            request.setBody(ShenyuRequest.Body.create(getRequestBody(paramMetadataList, args)));
        }
        return request;
    }

    private String urlJoinParams(final String url, final List<RequestTemplate.ParamMetadata> paramMetadataList, final Object[] args) {
        boolean isFistParam = !url.contains("=");
        final StringBuilder urlResult = new StringBuilder(url);
        for (RequestTemplate.ParamMetadata paramMetadata : paramMetadataList) {
            Annotation[] paramAnnotations = paramMetadata.getParamAnnotations();
            if (paramAnnotations != null && paramAnnotations.length > 0) {
                for (Annotation anno : paramAnnotations) {
                    if (anno instanceof RequestParam) {
                        if (!url.startsWith("?")) {
                            urlResult.append("?");
                        }
                        if (!isFistParam) {
                            urlResult.append("&");
                        }
                        isFistParam = false;
                        urlResult.append(((RequestParam) anno).value());
                        urlResult.append("=");
                        urlResult.append(args[paramMetadata.getParamIndexOnMethod()]);
                    }
                }
            }
        }
        return url;
    }

    private Object getRequestBody(final List<RequestTemplate.ParamMetadata> paramMetadataList, final Object[] args) {
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

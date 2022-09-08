package org.apache.shenyu.sdk.starter.core.support;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
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
            request.setPath(pathJoinParams(request.getPath(), paramMetadataList, args));
        }
        if (request.getHttpMethod() == ShenyuRequest.HttpMethod.POST) {
            request.setBody(JsonUtils.toJson(this.getRequestBody(paramMetadataList, args)));
        }
        return request;
    }

    private String pathJoinParams(final String path, final List<RequestTemplate.ParamMetadata> paramMetadataList, final Object[] args) {
        boolean isFistParam = !path.contains("=");
        final StringBuilder pathResult = new StringBuilder(path);
        for (RequestTemplate.ParamMetadata paramMetadata : paramMetadataList) {
            Annotation[] paramAnnotations = paramMetadata.getParamAnnotations();
            if (paramAnnotations == null || paramAnnotations.length == 0) {
                continue;
            }
            for (Annotation anno : paramAnnotations) {
                if (anno instanceof RequestParam) {
                    if (!path.startsWith("?")) {
                        pathResult.append("?");
                    }
                    if (!isFistParam) {
                        pathResult.append("&");
                    }
                    isFistParam = false;
                    pathResult.append(((RequestParam) anno).value());
                    pathResult.append("=");
                    pathResult.append(args[paramMetadata.getParamIndexOnMethod()]);
                }
            }
        }
        return pathResult.toString();
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

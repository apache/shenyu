package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class HttpApiDocRegistrar extends AbstractApiDocRegistrar {

    public HttpApiDocRegistrar(ShenyuClientRegisterEventPublisher publisher, ClientRegisterConfig clientRegisterConfig) {
        super(publisher, clientRegisterConfig);
    }

    @Override
    protected HttpApiSpecificInfo doParse(ApiBean.ApiDefinition apiDefinition) {

        RequestMapping requestMapping = apiDefinition.getAnnotation(RequestMapping.class);

        String produce = requestMapping.produces().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.produces());
        String consume = requestMapping.consumes().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.consumes());

        RequestMethod[] requestMethods = requestMapping.method();
        if (requestMethods.length == 0) {
            requestMethods = RequestMethod.values();
        }

        List<ApiHttpMethodEnum> apiHttpMethodEnums =
                Stream.of(requestMethods)
                        .map(item -> ApiHttpMethodEnum.of(item.name()))
                        .collect(Collectors.toList());

        return new HttpApiSpecificInfo(produce, consume, apiHttpMethodEnums);
    }
}

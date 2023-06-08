package org.apache.shenyu.client.core.register.registrar;

import com.google.common.collect.Lists;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;

import java.util.List;

public final class NoHttpApiDocRegistrar extends AbstractApiDocRegistrar {

    public NoHttpApiDocRegistrar(ShenyuClientRegisterEventPublisher publisher, ClientRegisterConfig clientRegisterConfig) {
        super(publisher, clientRegisterConfig);
    }

    @Override
    protected HttpApiSpecificInfo doParse(final ApiBean.ApiDefinition apiDefinition) {

        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;

        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;

        List<ApiHttpMethodEnum> apiHttpMethodEnums = Lists.newArrayList(ApiHttpMethodEnum.NOT_HTTP);

        return new HttpApiSpecificInfo(produce, consume, apiHttpMethodEnums);
    }
}

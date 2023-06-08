package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

public abstract class AbstractApiMetaRegistrar extends AbstractApiRegistrar<MetaDataRegisterDTO>{

    protected AbstractApiMetaRegistrar(ShenyuClientRegisterEventPublisher publisher) {
        super(publisher);
    }
}

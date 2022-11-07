/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.alibaba.dubbo.proxy;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.remoting.exchange.ResponseFuture;
import com.alibaba.dubbo.rpc.RpcContext;
import com.alibaba.dubbo.rpc.protocol.dubbo.FutureAdapter;
import com.alibaba.dubbo.rpc.service.GenericException;
import com.alibaba.dubbo.rpc.service.GenericService;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ParamCheckUtils;
import org.apache.shenyu.plugin.alibaba.dubbo.cache.AlibabaDubboConfigCache;
import org.apache.shenyu.plugin.dubbo.common.param.DubboParamResolveService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * Alibaba dubbo proxy service is  use GenericService.
 */
public class AlibabaDubboProxyService {

    private static final Logger LOG = LoggerFactory.getLogger(AlibabaDubboProxyService.class);

    private final DubboParamResolveService dubboParamResolveService;

    /**
     * Instantiates a new Dubbo proxy service.
     *
     * @param dubboParamResolveService the generic param resolve service
     */
    public AlibabaDubboProxyService(final DubboParamResolveService dubboParamResolveService) {
        this.dubboParamResolveService = dubboParamResolveService;
    }

    /**
     * Generic invoker object.
     *
     * @param body     the body
     * @param metaData the meta data
     * @return the object
     * @throws ShenyuException the shenyu exception
     */
    public ResponseFuture genericInvoker(final String body, final MetaData metaData) throws ShenyuException {
        ReferenceConfig<GenericService> reference = AlibabaDubboConfigCache.getInstance().get(metaData.getPath());
        if (Objects.isNull(reference) || StringUtils.isEmpty(reference.getInterface())) {
            AlibabaDubboConfigCache.getInstance().invalidate(metaData.getPath());
            reference = AlibabaDubboConfigCache.getInstance().initRef(metaData);
        }
        try {
            GenericService genericService = reference.get();
            Pair<String[], Object[]> pair;
            if (StringUtils.isBlank(metaData.getParameterTypes()) || ParamCheckUtils.bodyIsEmpty(body)) {
                pair = new ImmutablePair<>(new String[]{}, new Object[]{});
            } else {
                pair = dubboParamResolveService.buildParameter(body, metaData.getParameterTypes());
            }
            genericService.$invoke(metaData.getMethodName(), pair.getLeft(), pair.getRight());
        } catch (GenericException e) {
            LOG.error("dubbo invoker have exception", e);
            throw new ShenyuException(e.getExceptionMessage());
        }

        FutureAdapter<?> adapter = (FutureAdapter<?>) RpcContext.getContext().getFuture();
        return adapter.getFuture();
    }
}

/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.dubbo;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.rpc.service.GenericException;
import com.alibaba.dubbo.rpc.service.GenericService;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.exception.SoulException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
public class DubboProxyService {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboProxyService.class);

    private final GenericParamResolveService genericParamResolveService;

    /**
     * Instantiates a new Dubbo proxy service.
     *
     * @param genericParamResolveService the generic param resolve service
     */
    public DubboProxyService(final GenericParamResolveService genericParamResolveService) {
        this.genericParamResolveService = genericParamResolveService;
    }

    /**
     * Generic invoker object.
     *
     * @param body            the body
     * @param metaData        the meta data
     * @param dubboRuleHandle the dubbo rule handle
     * @return the object
     * @throws SoulException the soul exception
     */
    public Object genericInvoker(final String body, final MetaData metaData, final DubboRuleHandle dubboRuleHandle) throws SoulException {
        ReferenceConfig<GenericService> reference;
        GenericService genericService;
        try {
            reference = ApplicationConfigCache.getInstance().get(metaData.getServiceName());
            if (Objects.isNull(reference) || StringUtils.isEmpty(reference.getInterface())) {
                ApplicationConfigCache.getInstance().invalidate(metaData.getServiceName());
                reference = ApplicationConfigCache.getInstance().initRef(metaData);
            }
            genericService = reference.get();
        } catch (Exception ex) {
            LOGGER.error("dubbo 泛化初始化异常:", ex);
            ApplicationConfigCache.getInstance().invalidate(metaData.getServiceName());
            reference = ApplicationConfigCache.getInstance().initRef(metaData);
            genericService = reference.get();
        }
        try {
            if ("".equals(body) || "{}".equals(body) || "null".equals(body)) {
                return genericService.$invoke(metaData.getMethodName(), new String[]{}, new Object[]{});
            } else {
                Pair<String[], Object[]> pair = genericParamResolveService.buildParameter(body, metaData.getParameterTypes());
                return genericService.$invoke(metaData.getMethodName(), pair.getLeft(), pair.getRight());
            }
        } catch (GenericException e) {
            LOGGER.error("dubbo 泛化调用异常", e);
            throw new SoulException(e.getMessage());
        }
    }

}

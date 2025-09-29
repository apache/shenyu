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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;

import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** AiProxyApiKeyDataHandler. */
public class AiProxyApiKeyDataHandler extends AbstractDataHandler<ProxyApiKeyData> {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyDataHandler.class);

    private final List<AiProxyApiKeyDataSubscriber> subscribers;

    public AiProxyApiKeyDataHandler(final List<AiProxyApiKeyDataSubscriber> subscribers) {
        this.subscribers = subscribers;
    }

    @Override
    protected List<ProxyApiKeyData> convert(final String json) {
        return GsonUtils.getInstance().fromList(json, ProxyApiKeyData.class);
    }

    @Override
    protected void doRefresh(final List<ProxyApiKeyData> dataList) {
        LOG.info("[AiProxySync] DataHandler REFRESH, subscribers={}, items={}",
                Objects.isNull(subscribers) ? 0 : subscribers.size(),
                Objects.isNull(dataList) ? 0 : dataList.size());
        if (Objects.nonNull(subscribers)) {
            for (AiProxyApiKeyDataSubscriber s : subscribers) {
                s.refresh();
            }
        }
        doUpdate(dataList);
    }

    @Override
    protected void doUpdate(final List<ProxyApiKeyData> dataList) {
        if (Objects.isNull(dataList) || Objects.isNull(subscribers)) {
            return;
        }
        LOG.info("[AiProxySync] DataHandler UPDATE, subscribers={}, items={}", subscribers.size(), dataList.size());
        for (ProxyApiKeyData data : dataList) {
            LOG.info("[AiProxySync] onSubscribe proxyKey={}, enabled={}",
                    Objects.isNull(data) ? null : data.getProxyApiKey(),
                    Objects.isNull(data) ? null : data.getEnabled());
            for (AiProxyApiKeyDataSubscriber s : subscribers) {
                s.onSubscribe(data);
            }
        }
    }

    @Override
    protected void doDelete(final List<ProxyApiKeyData> dataList) {
        if (Objects.isNull(dataList) || Objects.isNull(subscribers)) {
            return;
        }
        for (ProxyApiKeyData data : dataList) {
            for (AiProxyApiKeyDataSubscriber s : subscribers) {
                s.unSubscribe(data);
            }
        }
    }
}
 
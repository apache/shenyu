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

package org.apache.shenyu.admin.listener.zookeeper;

import com.google.common.annotations.VisibleForTesting;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperConfig;
import org.apache.zookeeper.CreateMode;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Http service discovery.
 *
 * @deprecated sice 2.2.0  Deprecated
 */
@Component
@SuppressWarnings("all")
@Deprecated
public class HttpServiceDiscovery implements InitializingBean {

    public static final String ROOT = "/shenyu/register";

    public static final String URI_PATH = "/shenyu/register/*/*";

    private ZookeeperClient zkClient;

    private final SelectorService selectorService;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final Environment env;

    private volatile List<String> contextPathList;

    /**
     * Instantiates a new Http service discovery.
     *
     * @param selectorService the selector service
     * @param selectorMapper  the selector mapper
     * @param eventPublisher  the event publisher
     * @param env             the env
     */
    @Autowired(required = false)
    public HttpServiceDiscovery(final SelectorService selectorService,
                                final SelectorMapper selectorMapper,
                                final ApplicationEventPublisher eventPublisher,
                                final Environment env) {
        this.selectorService = selectorService;
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
        this.env = env;
    }

    @Override
    public void afterPropertiesSet() {
        Boolean register = env.getProperty("shenyu.http.register", Boolean.class, false);
        if (!register) {
            return;
        }
        String zookeeperUrl = env.getProperty("shenyu.http.zookeeperUrl", "");
        if (StringUtils.isNoneBlank(zookeeperUrl)) {
            zkClient = createZkClient(zookeeperUrl);
            boolean exists = zkClient.isExist(ROOT);
            if (!exists) {
                zkClient.createOrUpdate(ROOT, "", CreateMode.PERSISTENT);
            }

            zkClient.addCache(ROOT, new HttpServiceListener());
        }
    }

    @VisibleForTesting
    ZookeeperClient createZkClient(final String zookeeperUrl) {
        ZookeeperConfig config = new ZookeeperConfig(zookeeperUrl);
        config.setSessionTimeoutMilliseconds(5000)
                .setConnectionTimeoutMilliseconds(2000);
        ZookeeperClient client = new ZookeeperClient(config);
        client.start();
        return client;
    }

    private void updateServiceList(final List<String> children, final String contextPath) {
        List<String> uriList = new ArrayList<>();
        for (String subNode : children) {
            // Read node data
            String data = zkClient.get(subNode);
            uriList.add(data);
        }
        updateSelectorHandler(contextPath, uriList);
    }

    private void updateSelectorHandler(final String contextPath, final List<String> uriList) {
        SelectorDO selector = selectorService.findByName(contextPath);
        if (Objects.nonNull(selector)) {
            SelectorData selectorData = selectorService.buildByName(contextPath);
            if (uriList == null) {
                selector.setHandle("");
                selectorData.setHandle("");
            } else {
                String handler = GsonUtils.getInstance().toJson(buildDivideUpstream(uriList));
                selector.setHandle(handler);
                selectorData.setHandle(handler);
            }
            selectorMapper.updateSelective(selector);

            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                    Collections.singletonList(selectorData)));
        }
    }

    private List<DivideUpstream> buildDivideUpstream(final List<String> uriList) {
        return uriList.stream().map(uri -> {
            DivideUpstream divideUpstream = DivideUpstream.builder()
                    .upstreamHost("localhost")
                    .protocol("http://")
                    .upstreamUrl(uri)
                    .weight(50)
                    .build();
            return divideUpstream;
        }).collect(Collectors.toList());
    }

    class HttpServiceListener implements TreeCacheListener {
        @Override
        public void childEvent(final CuratorFramework client, final TreeCacheEvent event) throws Exception {
            String path = event.getData().getPath();
            // if not uri register path, return.
            if (!path.contains(ROOT)) {
                return;
            }

            // get children under context path
            int lastSepIndex = path.lastIndexOf(Constants.PATH_SEPARATOR);
            String contextPath = path.substring(0, lastSepIndex);
            List<String> childrenList = zkClient.getChildren(contextPath);
            List<String> collect = childrenList.stream().map(r -> contextPath + "/" + r).collect(Collectors.toList());
            updateServiceList(collect, contextPath);
        }
    }
}

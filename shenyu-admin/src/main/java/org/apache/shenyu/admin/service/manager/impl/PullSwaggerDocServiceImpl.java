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

package org.apache.shenyu.admin.service.manager.impl;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.service.manager.PullSwaggerDocService;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * ServiceDocManagerImpl.
 */
@Service
public class PullSwaggerDocServiceImpl implements PullSwaggerDocService {
    private static final Logger LOG = LoggerFactory.getLogger(PullSwaggerDocServiceImpl.class);

    private static final HttpUtils HTTP_UTILS = new HttpUtils();

    private static final String SWAGGER_V2_PATH = "/v2/api-docs";

    @Resource
    private DocManager docManager;

    @Resource
    private TagService tagService;

    @Override
    public void pullApiDocument(final Set<UpstreamInstance> currentServices) {
        currentServices.forEach(this::pullApiDocument);
    }

    /**
     * pullApiDocument.
     *
     * @param instance UpstreamInstance.
     */
    @Override
    @SuppressWarnings("unchecked")
    public void pullApiDocument(final UpstreamInstance instance) {
        String clusterName = instance.getClusterName();
        TagDO.TagExt tagExt = getTagExt(instance);
        if (!canPull(instance, tagExt.getRefreshTime())) {
            LOG.info("api document has been pulled and cannot be pulled again，instance: {}", JsonUtils.toJson(instance));
            return;
        }
        long newRefreshTime = System.currentTimeMillis();
        String url = getSwaggerRequestUrl(instance);
        try {
            String body = HTTP_UTILS.get(url, Collections.EMPTY_MAP);
            docManager.addDocInfo(
                clusterName,
                body,
                tagExt.getApiDocMd5(),
                callback -> {
                    LOG.info("save api document successful，clusterName={}, ipPort={}", clusterName, instance.getIp() + ":" + instance.getPort());
                    tagExt.setRefreshTime(newRefreshTime);
                    tagExt.setApiDocMd5(callback.getDocMd5());
                }
            );
            //Save the time of the last updated document and the newMd5 of apidoc.
            tagService.updateTagExt(tagExt.getId(), tagExt);
        } catch (Exception e) {
            LOG.error("add api document fail. url={} error={}", url, e);
        }
    }

    private boolean canPull(final UpstreamInstance instance, final Long cacheLastStartUpTime) {
        boolean canPull = false;
        if (Objects.isNull(cacheLastStartUpTime) || instance.getStartupTime() > cacheLastStartUpTime) {
            canPull = true;
        }
        return canPull;
    }

    private TagDO.TagExt getTagExt(final UpstreamInstance instance) {
        TagDO.TagExt tagExt = null;
        List<TagVO> tagVOList = tagService.findByQuery(instance.getContextPath(), AdminConstants.TAG_ROOT_PARENT_ID);
        if (CollectionUtils.isNotEmpty(tagVOList)) {
            String ext = tagVOList.get(0).getExt();
            tagExt = StringUtils.isNotEmpty(ext) ? JsonUtils.jsonToObject(ext, TagDO.TagExt.class) : null;
        }
        return Optional.ofNullable(tagExt).orElse(new TagDO.TagExt());
    }

    private String getSwaggerRequestUrl(final UpstreamInstance instance) {
        return "http://" + instance.getIp() + ":" + instance.getPort() + SWAGGER_V2_PATH;

    }

}

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

package org.apache.shenyu.admin.jpa.repository;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private ApiRepository apiRepository;

    @Resource
    private TagRepository tagRepository;

    @Resource
    private TagRelationRepository tagRelationRepository;

    @Test
    void pageByQueryWithTagIdJoinsTagRelation() {
        TagDO tagDO = tagRepository.save(buildTag("join-tag"));
        ApiDO first = apiRepository.save(buildApi("/join/api/1"));
        ApiDO second = apiRepository.save(buildApi("/join/api/2"));
        bindTag(first.getId(), tagDO.getId());

        ApiQuery byTag = new ApiQuery();
        byTag.setTagId(tagDO.getId());
        List<ApiDO> tagged = apiRepository.pageByQuery(byTag, Pageable.unpaged()).getContent();
        assertEquals(1, tagged.size());
        assertEquals(first.getId(), tagged.get(0).getId());

        ApiQuery byPath = new ApiQuery();
        byPath.setApiPath("join/api");
        assertEquals(2, apiRepository.pageByQuery(byPath, Pageable.unpaged()).getTotalElements());

        ApiQuery byPathAndTag = new ApiQuery();
        byPathAndTag.setApiPath("join/api/2");
        byPathAndTag.setTagId(tagDO.getId());
        assertTrue(apiRepository.pageByQuery(byPathAndTag, Pageable.unpaged()).isEmpty());
    }

    @Test
    void tagAndTagRelationScopedQueriesAndDeletes() {
        TagDO tagA = tagRepository.save(buildTag("scope-tag-a"));
        TagDO tagB = tagRepository.save(buildTag("scope-tag-b"));
        ApiDO api = apiRepository.save(buildApi("/scope/api"));
        final TagRelationDO relationA = bindTag(api.getId(), tagA.getId());
        bindTag(api.getId(), tagB.getId());

        assertEquals(2, tagRelationRepository.findByApiId(api.getId()).size());
        assertEquals(1, tagRelationRepository.findByTagId(tagA.getId()).size());

        ApiQuery query = new ApiQuery();
        query.setTagId(tagA.getId());
        assertEquals(1, apiRepository.pageByQuery(query, Pageable.unpaged()).getTotalElements());

        assertEquals(2, tagRelationRepository.deleteByApiId(api.getId()));
        assertTrue(tagRelationRepository.findByApiId(api.getId()).isEmpty());
        assertTrue(tagRelationRepository.findById(relationA.getId()).isEmpty());

        tagRelationRepository.save(relationA);
        assertEquals(1, tagRelationRepository.deleteByApiIds(List.of(api.getId())));
    }

    private TagDO buildTag(final String name) {
        TagDO tagDO = new TagDO();
        tagDO.setId(UUIDUtils.getInstance().generateShortUuid());
        tagDO.setName(name);
        tagDO.setTagDesc("tag desc for " + name);
        tagDO.setParentTagId("0");
        tagDO.setExt("{}");
        return tagDO;
    }

    private TagRelationDO bindTag(final String apiId, final String tagId) {
        TagRelationDO relationDO = new TagRelationDO();
        relationDO.setId(UUIDUtils.getInstance().generateShortUuid());
        relationDO.setApiId(apiId);
        relationDO.setTagId(tagId);
        return tagRelationRepository.save(relationDO);
    }

    private ApiDO buildApi(final String apiPath) {
        ApiDO apiDO = new ApiDO();
        apiDO.setId(UUIDUtils.getInstance().generateShortUuid());
        apiDO.setContextPath(apiPath.substring(0, apiPath.lastIndexOf('/')));
        apiDO.setApiPath(apiPath);
        apiDO.setHttpMethod(0);
        apiDO.setRpcType("http");
        apiDO.setState(0);
        apiDO.setVersion("1.0.0");
        apiDO.setConsume("*/*");
        apiDO.setProduce("*/*");
        apiDO.setExt("{}");
        apiDO.setApiOwner("tester");
        apiDO.setApiDesc("test api");
        apiDO.setApiSource(2);
        apiDO.setDocument("{}");
        apiDO.setDocumentMd5("md5");
        return apiDO;
    }
}

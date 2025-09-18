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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;
import org.apache.shenyu.common.utils.DateUtils;

import java.util.Optional;

/**
 * ProxyApiKeyTransfer.
 */
public enum ProxyApiKeyTransfer {

    /**
     * Instance.
     */
    INSTANCE;

    /**
     * Map DTO to Entity.
     *
     * @param dto dto
     * @return do entity
     */
    public ProxyApiKeyDO mapToEntity(final ProxyApiKeyDTO dto) {
        return Optional.ofNullable(dto)
                .map(v -> {
                    ProxyApiKeyDO e = new ProxyApiKeyDO(v.getProxyApiKey(), v.getDescription(), v.getEnabled(),
                            v.getNamespaceId());
                    e.setId(v.getId());
                    return e;
                })
                .orElse(null);
    }

    /**
     * Map DO to VO.
     *
     * @param entity entity
     * @return vo
     */
    public ProxyApiKeyVO mapToVO(final ProxyApiKeyDO entity) {
        return Optional.ofNullable(entity)
                .map(v -> {
                    ProxyApiKeyVO vo = new ProxyApiKeyVO();
                    vo.setId(v.getId());
                    vo.setProxyApiKey(v.getProxyApiKey());
                    vo.setDescription(v.getDescription());
                    vo.setEnabled(v.getEnabled());
                    vo.setNamespaceId(v.getNamespaceId());
                    vo.setSelectorId(v.getSelectorId());
                    vo.setDateUpdated(Optional.ofNullable(v.getDateUpdated())
                            .map(u -> DateUtils.localDateTimeToString(u.toLocalDateTime()))
                            .orElse(null));
                    return vo;
                })
                .orElse(null);
    }
}
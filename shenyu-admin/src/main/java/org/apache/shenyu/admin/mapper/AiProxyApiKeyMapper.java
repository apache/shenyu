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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * AiProxyApiKeyMapper.
 */
@Mapper
public interface AiProxyApiKeyMapper extends ExistProvider {

    @Override
    Boolean existed(@Param("id") Serializable id);

    Boolean proxyApiKeyExisted(@Param("selectorId") String selectorId, @Param("proxyApiKey") String proxyApiKey);

    ProxyApiKeyDO selectById(String id);

    List<ProxyApiKeyDO> selectByIds(@Param("ids") List<String> ids);

    /**
     * select proxy api key by selector id.
     *
     * @param selectorId selector id.
     * @return proxy api keys.
     */
    List<ProxyApiKeyDO> selectBySelectorId(@Param("selectorId") String selectorId);

    List<ProxyApiKeyDO> selectAll();

    List<ProxyApiKeyDO> selectAllByNamespaceId(String namespaceId);

    List<ProxyApiKeyDO> selectByQuery(ProxyApiKeyQuery query);

    Integer countByQuery(ProxyApiKeyQuery query);

    int insert(ProxyApiKeyDO entity);

    int insertSelective(ProxyApiKeyDO entity);

    int update(ProxyApiKeyDO entity);

    int updateSelective(ProxyApiKeyDO entity);

    int updateEnableBatch(@Param("idList") List<String> idList, @Param("enabled") Boolean enabled);

    int delete(String id);

    int deleteByIds(List<String> ids);

    List<ProxyApiKeyVO> selectByCondition(@Param("condition") ProxyApiKeyQuery condition);
}
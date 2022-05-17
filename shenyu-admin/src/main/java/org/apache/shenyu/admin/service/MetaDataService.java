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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.List;
import java.util.Map;

/**
 * this is application authority service.
 */
public interface MetaDataService {
    
    /**
     * save or update mate data.
     * {@link org.apache.shenyu.admin.service.register.AbstractShenyuClientRegisterServiceImpl}
     *
     * @param exist       has been exist meta data {@link MetaDataDO}
     * @param metaDataDTO meta data dto {@link MetaDataRegisterDTO}
     */
    void saveOrUpdateMetaData(MetaDataDO exist, MetaDataRegisterDTO metaDataDTO);
    
    /**
     * Create or update int.
     *
     * @param metaDataDTO the meta data dto
     * @return the String
     */
    String createOrUpdate(MetaDataDTO metaDataDTO);
    
    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return rows int
     */
    int delete(List<String> ids);
    
    /**
     * Find by id meta data vo.
     *
     * @param id the id
     * @return the meta data vo
     */
    MetaDataVO findById(String id);
    
    /**
     * List by page common pager.
     *
     * @param metaDataQuery the meta data query
     * @return the common pager
     */
    CommonPager<MetaDataVO> listByPage(MetaDataQuery metaDataQuery);
    
    /**
     * Find all list.
     *
     * @return the list
     */
    List<MetaDataVO> findAll();
    
    /**
     * Find all group map.
     *
     * @return the map
     */
    Map<String, List<MetaDataVO>> findAllGroup();
    
    /**
     * List all list.
     *
     * @return the list
     */
    List<MetaData> listAll();
    
    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return the string
     */
    String enabled(List<String> ids, Boolean enabled);
    
    /**
     * Sync data.
     */
    void syncData();
    
    /**
     * find meta data by path.
     *
     * @param path the path of meta data
     * @return {@link MetaDataDO}
     */
    MetaDataDO findByPath(String path);
    
    /**
     * find meta data by service's name and method's name.
     *
     * @param serviceName the name of service
     * @param methodName  the name of method
     * @return {@link MetaDataDO}
     */
    MetaDataDO findByServiceNameAndMethodName(String serviceName, String methodName);
    
    /**
     * insert MetaDataDO.
     *
     * @param metaDataDO meta data object
     * @return the success rows.
     */
    int insert(MetaDataDO metaDataDO);
}

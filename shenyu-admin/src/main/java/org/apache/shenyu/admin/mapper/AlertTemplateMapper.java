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
import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;

import java.util.List;

/**
 * AlertTemplateMapper.
 */
@Mapper
public interface AlertTemplateMapper {

    /**
     * delete by ids.
     * @param ids ids
     * @return rows int
     */
    int deleteByIds(List<String> ids);

    /**
     * insert selective alertTemplateDTO.
     * @param alertTemplateDTO {@linkplain AlertTemplateDTO}
     * @return rows int
     */
    int insertSelective(AlertTemplateDTO alertTemplateDTO);

    /**
     * select by id.
     * @param id id
     * @return {@linkplain AlertTemplateDO}
     */
    AlertTemplateDO selectByPrimaryKey(Long id);

    /**
     * update selective alertTemplate by id.
     * @param alertTemplateDTO {@linkplain AlertTemplateDTO}
     * @return rows int
     */
    int updateByPrimaryKeySelective(AlertTemplateDTO alertTemplateDTO);

    /**
     * select all.
     * @return list of AlertTemplateVO
     */
    List<AlertTemplateVO> selectAll();
}

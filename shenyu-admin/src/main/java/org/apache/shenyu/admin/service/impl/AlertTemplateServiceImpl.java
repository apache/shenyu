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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.AlertTemplateMapper;
import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;
import org.apache.shenyu.admin.service.AlertTemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Implementation of the {@link AlertTemplateService}.
 */
@Service
public class AlertTemplateServiceImpl implements AlertTemplateService {

    @Autowired
    private AlertTemplateMapper alertTemplateMapper;

    @Override
    public int addTemplate(final AlertTemplateDTO alertTemplateDTO) {

        return alertTemplateMapper.insertSelective(alertTemplateDTO);
    }

    @Override
    public int deleteTemplate(final List<String> ids) {
        return alertTemplateMapper.deleteByIds(ids);
    }

    @Override
    public int updateTemplate(final AlertTemplateDTO alertTemplateDTO) {
        return alertTemplateMapper.updateByPrimaryKeySelective(alertTemplateDTO);
    }

    @Override
    public List<AlertTemplateVO> getAll() {
        return alertTemplateMapper.selectAll();
    }

    @Override
    public AlertTemplateDO detail(final Long id) {
        return alertTemplateMapper.selectByPrimaryKey(id);
    }
}

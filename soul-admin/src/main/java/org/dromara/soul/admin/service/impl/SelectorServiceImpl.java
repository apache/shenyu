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

package org.dromara.soul.admin.service.impl;

import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.vo.SelectorVO;
import org.springframework.stereotype.Service;

/**
 * SelectorServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("selectorService")
public class SelectorServiceImpl implements SelectorService {

    /**
     * create or update selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows
     */
    @Override
    public int createOrUpdate(final SelectorDTO selectorDTO) {
        return 0;
    }

    /**
     * enabled or disabled selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows
     */
    @Override
    public int enabled(final SelectorDTO selectorDTO) {
        return 0;
    }

    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorDO}
     */
    @Override
    public SelectorDO findById(final String id) {
        return null;
    }

    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<SelectorVO> listByPage(final SelectorQuery selectorQuery) {
        return null;
    }
}

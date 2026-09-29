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

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.jpa.repository.ShenyuDictRepository;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.publish.DictEventPublisher;
import org.apache.shenyu.admin.utils.Assert;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ShenyuDictService}.
 */
@Service
public class ShenyuDictServiceImpl implements ShenyuDictService {
    
    private final ShenyuDictRepository shenyuDictRepository;

    private final DictEventPublisher publisher;

    public ShenyuDictServiceImpl(final ShenyuDictRepository shenyuDictRepository,
                                  final DictEventPublisher publisher) {
        this.shenyuDictRepository = shenyuDictRepository;
        this.publisher = publisher;
    }
    
    @Override
    public CommonPager<ShenyuDictVO> listByPage(final ShenyuDictQuery shenyuDictQuery) {
        return PageResultUtils.result(shenyuDictQuery.getPageParameter(),
                shenyuDictRepository.selectByQuery(shenyuDictQuery, PageResultUtils.of(shenyuDictQuery.getPageParameter())),
                ShenyuDictVO::buildShenyuDictVO);
    }
    
    @Override
    public Integer createOrUpdate(final ShenyuDictDTO shenyuDictDTO) {
        return StringUtils.isBlank(shenyuDictDTO.getId()) ? create(shenyuDictDTO) : update(shenyuDictDTO);
    }
    
    private int update(final ShenyuDictDTO shenyuDictDTO) {
        final ShenyuDictDO dict = ShenyuDictDO.buildShenyuDictDO(shenyuDictDTO);
        final ShenyuDictDO before = shenyuDictRepository.findById(shenyuDictDTO.getId()).orElse(null);
        Assert.notNull(before, "the dict is not existed");
        final ShenyuDictDO snapshot = copyOf(before);
        if (Objects.nonNull(dict.getType())) {
            before.setType(dict.getType());
        }
        if (Objects.nonNull(dict.getDictCode())) {
            before.setDictCode(dict.getDictCode());
        }
        if (Objects.nonNull(dict.getDictName())) {
            before.setDictName(dict.getDictName());
        }
        if (Objects.nonNull(dict.getDictValue())) {
            before.setDictValue(dict.getDictValue());
        }
        if (Objects.nonNull(dict.getDesc())) {
            before.setDesc(dict.getDesc());
        }
        if (Objects.nonNull(dict.getSort())) {
            before.setSort(dict.getSort());
        }
        if (Objects.nonNull(dict.getEnabled())) {
            before.setEnabled(dict.getEnabled());
        }
        before.setDateUpdated(dict.getDateUpdated());
        shenyuDictRepository.save(before);
        publisher.onUpdated(before, snapshot);
        return 1;
    }
    
    private int create(final ShenyuDictDTO shenyuDictDTO) {
        final ShenyuDictDO dict = ShenyuDictDO.buildShenyuDictDO(shenyuDictDTO);
        shenyuDictRepository.save(dict);
        publisher.onCreated(dict);
        return 1;
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deleteShenyuDicts(final List<String> ids) {
        final List<ShenyuDictDO> dictList = shenyuDictRepository.findAllById(ids);
        if (dictList.isEmpty()) {
            return 0;
        }
        shenyuDictRepository.deleteAllByIdInBatch(ids);
        publisher.onDeleted(dictList);
        return dictList.size();
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer enabled(final List<String> ids, final Boolean enabled) {
        return shenyuDictRepository.enabled(ids, enabled);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<ShenyuDictDTO> dictList) {
        if (CollectionUtils.isEmpty(dictList)) {
            return ConfigImportResult.success();
        }
        Map<String, List<ShenyuDictVO>> dictTypeMap = listAllData()
                .stream()
                .collect(Collectors.groupingBy(ShenyuDictVO::getType));
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        for (ShenyuDictDTO dictDTO : dictList) {
            String type = dictDTO.getType();
            String dictName = dictDTO.getDictName();
            Set<String> existDictNameSet = dictTypeMap
                    .getOrDefault(type, Lists.newArrayList())
                    .stream()
                    .map(ShenyuDictVO::getDictName)
                    .collect(Collectors.toSet());
            // check if dictName exists for this type
            if (existDictNameSet.contains(dictName)) {
                errorMsgBuilder
                        .append(dictName)
                        .append(",");
                continue;
            }
            create(dictDTO);
            successCount++;
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail dict: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }

    @Override
    public ShenyuDictVO findById(final String id) {
        return ShenyuDictVO.buildShenyuDictVO(shenyuDictRepository.findById(id).orElse(null));
    }
    
    @Override
    public ShenyuDictVO findByDictCodeName(final String dictCode, final String dictName) {
        return ShenyuDictVO.buildShenyuDictVO(shenyuDictRepository.findByDictCodeAndDictName(dictCode, dictName).orElse(null));
    }
    
    @Override
    public List<ShenyuDictVO> list(final String type) {
        return shenyuDictRepository.findByType(type).stream()
                .map(ShenyuDictVO::buildShenyuDictVO)
                .collect(Collectors.toList());
    }

    @Override
    public List<ShenyuDictVO> listAllData() {
        return shenyuDictRepository.findAll().stream()
                .map(ShenyuDictVO::buildShenyuDictVO)
                .collect(Collectors.toList());
    }

    /**
     * Detached snapshot of a managed entity, so change events keep a stable before-image.
     *
     * @param dictDO the managed entity
     * @return the snapshot copy
     */
    private ShenyuDictDO copyOf(final ShenyuDictDO dictDO) {
        ShenyuDictDO snapshot = new ShenyuDictDO();
        snapshot.setId(dictDO.getId());
        snapshot.setDateCreated(dictDO.getDateCreated());
        snapshot.setDateUpdated(dictDO.getDateUpdated());
        snapshot.setType(dictDO.getType());
        snapshot.setDictCode(dictDO.getDictCode());
        snapshot.setDictName(dictDO.getDictName());
        snapshot.setDictValue(dictDO.getDictValue());
        snapshot.setDesc(dictDO.getDesc());
        snapshot.setSort(dictDO.getSort());
        snapshot.setEnabled(dictDO.getEnabled());
        return snapshot;
    }

}

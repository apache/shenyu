/*
 *   Licensed to the Apache Software Foundation (final ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (final the "License"); you may not use this file except in compliance with
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

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorConditionMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.query.SelectorConditionQuery;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.constant.AdminConstants;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.ConditionZkDTO;
import org.dromara.soul.common.dto.zk.PluginZkDTO;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.dto.zk.SelectorZkDTO;
import org.dromara.soul.common.enums.PluginRoleEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * PluginServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("pluginService")
public class PluginServiceImpl implements PluginService {

    private final PluginMapper pluginMapper;

    private final SelectorMapper selectorMapper;

    private SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final ZkClient zkClient;

    @Autowired(required = false)
    public PluginServiceImpl(final PluginMapper pluginMapper,
                             final SelectorMapper selectorMapper,
                             final SelectorConditionMapper selectorConditionMapper,
                             final RuleMapper ruleMapper,
                             final RuleConditionMapper ruleConditionMapper,
                             final ZkClient zkClient) {
        this.pluginMapper = pluginMapper;
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.zkClient = zkClient;
    }

    /**
     * create or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final PluginDTO pluginDTO) {
        final String msg = checkData(pluginDTO);
        if (StringUtils.isNoneBlank(msg)) {
            return msg;
        }
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        if (StringUtils.isBlank(pluginDTO.getId())) {
            pluginMapper.insertSelective(pluginDO);
        } else {
            pluginMapper.updateSelective(pluginDO);
        }
        String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        zkClient.writeData(pluginPath, new PluginZkDTO(pluginDO.getId(),
                pluginDO.getName(), pluginDO.getRole(), pluginDO.getEnabled()));
        return "";
    }

    private String checkData(final PluginDTO pluginDTO) {
        final PluginDO exist = pluginMapper.selectByName(pluginDTO.getName());
        if (StringUtils.isBlank(pluginDTO.getId())) {
            if (Objects.nonNull(exist)) {
                return AdminConstants.PLUGIN_NAME_IS_EXIST;
            }
        } else {
            if (Objects.isNull(exist)) {
                return AdminConstants.PLUGIN_NAME_NOT_EXIST;
            }
            if (!exist.getId().equals(pluginDTO.getId())) {
                return AdminConstants.PLUGIN_NAME_IS_EXIST;
            }
        }
        return "";
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        for (String id : ids) {
            PluginDO pluginDO = pluginMapper.selectById(id);
            if (Objects.isNull(pluginDO)) {
                return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
            }
            // if sys plugin not delete
            if (pluginDO.getRole().equals(PluginRoleEnum.SYS.getCode())) {
                return AdminConstants.SYS_PLUGIN_NOT_DELETE;
            }
            pluginMapper.delete(id);

            final List<SelectorDO> selectorDOList = selectorMapper.selectByQuery(new SelectorQuery(id, null));
            selectorDOList.forEach(selectorDO -> {
                final List<RuleDO> ruleDOS = ruleMapper.selectByQuery(new RuleQuery(selectorDO.getId(), null));
                ruleDOS.forEach(ruleDO -> {
                    ruleMapper.delete(ruleDO.getId());
                    ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
                });
                selectorMapper.delete(selectorDO.getId());
                selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
            });

            String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
            if (zkClient.exists(pluginPath)) {
                zkClient.deleteRecursive(pluginPath);
            }
            String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginDO.getName());
            if (zkClient.exists(selectorParentPath)) {
                zkClient.deleteRecursive(selectorParentPath);
            }
            String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());
            if (zkClient.exists(ruleParentPath)) {
                zkClient.deleteRecursive(ruleParentPath);
            }
        }
        return "";
    }

    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {
        for (String id : ids) {
            PluginDO pluginDO = pluginMapper.selectById(id);
            if (Objects.isNull(pluginDO)) {
                return AdminConstants.SYS_PLUGIN_ID_NOT_EXIST;
            }
            pluginDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
            pluginDO.setEnabled(enabled);

            pluginMapper.updateEnable(pluginDO);

            String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
            if (!zkClient.exists(pluginPath)) {
                zkClient.createPersistent(pluginPath, true);
            }
            zkClient.writeData(pluginPath, new PluginZkDTO(pluginDO.getId(),
                    pluginDO.getName(), pluginDO.getRole(), pluginDO.getEnabled()));

        }
        return "";
    }


    /**
     * find plugin by id.
     *
     * @param id primary key.
     * @return {@linkplain PluginVO}
     */
    @Override
    public PluginVO findById(final String id) {
        return PluginVO.buildPluginVO(pluginMapper.selectById(id));
    }

    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<PluginVO> listByPage(final PluginQuery pluginQuery) {
        PageParameter pageParameter = pluginQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(),
                        pluginMapper.countByQuery(pluginQuery)),
                pluginMapper.selectByQuery(pluginQuery).stream()
                        .map(PluginVO::buildPluginVO)
                        .collect(Collectors.toList()));
    }

    /**
     * sync plugin.
     *
     * @param pluginId {@linkplain String}
     * @return isNull
     */
    @Override
    public int syncPluginData(final String pluginId) {
        PluginDO pluginDO = pluginMapper.selectById(pluginId);
        if (pluginDO != null) {
            syncPlugin(pluginDO);
            return 1;
        }
        return 0;
    }


    /**
     * sync plugins.
     *
     * @return rows
     */
    @Override
    public int syncPluginAll() {
        List<PluginDO> pluginDOs = pluginMapper.selectByQuery(new PluginQuery());
        if (CollectionUtils.isNotEmpty(pluginDOs)) {
            String pluginPath = ZkPathConstants.buildPluginParentPath();
            if (!zkClient.exists(pluginPath)) {
                zkClient.createPersistent(pluginPath, true);
            }
            List<String> pluginZKs = zkClient.getChildren(ZkPathConstants.buildPluginParentPath());
            pluginDOs.forEach(pluginDO -> {
                if (CollectionUtils.isNotEmpty(pluginZKs)) {
                    pluginZKs.remove(pluginDO.getName());
                }
                syncPlugin(pluginDO);
            });

            pluginZKs.forEach(pluginZK -> {
                zkClient.delete(ZkPathConstants.buildPluginPath(pluginZK));
                String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginZK);
                if (zkClient.exists(selectorParentPath)) {
                    zkClient.delete(selectorParentPath);
                }
                String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginZK);
                if (zkClient.exists(ruleParentPath)) {
                    zkClient.delete(ruleParentPath);
                }
            });
            return pluginDOs.size();
        }

        return 0;
    }


    /**
     * sync plugin.
     *
     * @param pluginDO {@linkplain PluginDO}
     */
    private void syncPlugin(final PluginDO pluginDO) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        zkClient.writeData(pluginPath, new PluginZkDTO(pluginDO.getId(),
                pluginDO.getName(), pluginDO.getRole(), pluginDO.getEnabled()));

        final String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginDO.getName());

        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }

        List<String> selectorZKs = zkClient.getChildren(selectorParentPath);
        selectorMapper.selectByQuery(new SelectorQuery(pluginDO.getId(), null)).forEach(selectorDO -> {
            if (CollectionUtils.isNotEmpty(selectorZKs)) {
                selectorZKs.remove(selectorDO.getId());
            }
            String selectorRealPath = ZkPathConstants.buildSelectorRealPath(pluginDO.getName(), selectorDO.getId());
            if (!zkClient.exists(selectorRealPath)) {
                zkClient.createPersistent(selectorRealPath, true);
            }
            List<ConditionZkDTO> selectorConditionZkDTOs = selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())).stream()
                    .map(selectorConditionDO -> new ConditionZkDTO(selectorConditionDO.getParamType(), selectorConditionDO.getOperator(),
                            selectorConditionDO.getParamName(), selectorConditionDO.getParamValue())).collect(Collectors.toList());
            zkClient.writeData(selectorRealPath, new SelectorZkDTO(selectorDO.getId(), selectorDO.getPluginId(), pluginDO.getName(),
                    selectorDO.getName(), selectorDO.getMatchMode(), selectorDO.getType(), selectorDO.getSort(), selectorDO.getEnabled(),
                    selectorDO.getLoged(), selectorDO.getContinued(), selectorDO.getHandle(), selectorConditionZkDTOs));

            final String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());

            if (!zkClient.exists(ruleParentPath)) {
                zkClient.createPersistent(ruleParentPath, true);
            }
            List<String> ruleZKs = zkClient.getChildren(ruleParentPath);
            ruleMapper.selectByQuery(new RuleQuery(selectorDO.getId(), null)).forEach(ruleDO -> {
                if (CollectionUtils.isNotEmpty(ruleZKs)) {
                    ruleZKs.remove(selectorDO.getId() + ZkPathConstants.SELECTOR_JOIN_RULE + ruleDO.getId());
                }
                String ruleRealPath = ZkPathConstants.buildRulePath(pluginDO.getName(), selectorDO.getId(), ruleDO.getId());
                if (!zkClient.exists(ruleRealPath)) {
                    zkClient.createPersistent(ruleRealPath, true);
                }
                List<ConditionZkDTO> ruleConditionZkDTOs = ruleConditionMapper.selectByQuery(new RuleConditionQuery(ruleDO.getId())).stream()
                        .map(ruleConditionDO -> new ConditionZkDTO(ruleConditionDO.getParamType(), ruleConditionDO.getOperator(),
                                ruleConditionDO.getParamName(), ruleConditionDO.getParamValue())).collect(Collectors.toList());
                zkClient.writeData(ruleRealPath, new RuleZkDTO(ruleDO.getId(), pluginDO.getName(), ruleDO.getSelectorId(),
                        ruleDO.getMatchMode(), ruleDO.getSort(), ruleDO.getEnabled(), ruleDO.getLoged(), ruleDO.getHandle(), ruleConditionZkDTOs));
            });

            ruleZKs.forEach(ruleZK -> zkClient.delete(ZkPathConstants.buildRulePath(pluginDO.getName(), selectorDO.getId(), ruleZK)));
        });

        selectorZKs.forEach(selectorZK -> {
            zkClient.delete(ZkPathConstants.buildSelectorRealPath(pluginDO.getName(), selectorZK));
            String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());
            zkClient.getChildren(ruleParentPath).forEach(selectorRulePath -> {
                if (selectorRulePath.split(ZkPathConstants.SELECTOR_JOIN_RULE)[0].equals(selectorZK)) {
                    zkClient.delete(ruleParentPath + "/" + selectorRulePath);
                }
            });
        });
    }
}

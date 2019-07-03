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

package org.dromara.soul.admin.service.sync;

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorConditionMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.query.SelectorConditionQuery;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.transfer.ConditionTransfer;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.PluginData;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Zk sync data service.
 *
 * @author xiaoyu(Myth)
 */
public class ZkSyncDataServiceImpl implements SyncDataService {

    private final ZkClient zkClient;

    private final PluginMapper pluginMapper;

    private final SelectorMapper selectorMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    /**
     * Instantiates a new Zk sync data service.
     *
     * @param zkClient                the zk client
     * @param pluginMapper            the plugin mapper
     * @param selectorMapper          the selector mapper
     * @param selectorConditionMapper the selector condition mapper
     * @param ruleMapper              the rule mapper
     * @param ruleConditionMapper     the rule condition mapper
     */
    public ZkSyncDataServiceImpl(final ZkClient zkClient,
                                 final PluginMapper pluginMapper,
                                 final SelectorMapper selectorMapper,
                                 final SelectorConditionMapper selectorConditionMapper,
                                 final RuleMapper ruleMapper,
                                 final RuleConditionMapper ruleConditionMapper) {
        this.zkClient = zkClient;
        this.pluginMapper = pluginMapper;
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
    }

    @Override
    public boolean syncAll() {

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
            return true;
        }
        return false;
    }

    @Override
    public boolean syncPluginData(String pluginId) {
        PluginDO pluginDO = pluginMapper.selectById(pluginId);
        if (Objects.isNull(pluginDO)) {
            return false;
        }
        syncPlugin(pluginDO);
        return true;
    }


    private void syncPlugin(final PluginDO pluginDO) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        zkClient.writeData(pluginPath, new PluginData(pluginDO.getId(),
                pluginDO.getName(), pluginDO.getConfig(), pluginDO.getRole(), pluginDO.getEnabled()));

        final String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginDO.getName());

        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }

        List<String> selectorZKs = zkClient.getChildren(selectorParentPath);
        selectorMapper.selectByQuery(new SelectorQuery(pluginDO.getId(), null))
                .forEach(selectorDO -> {
                    if (CollectionUtils.isNotEmpty(selectorZKs)) {
                        selectorZKs.remove(selectorDO.getId());
                    }
                    String selectorRealPath = ZkPathConstants.buildSelectorRealPath(pluginDO.getName(), selectorDO.getId());
                    if (!zkClient.exists(selectorRealPath)) {
                        zkClient.createPersistent(selectorRealPath, true);
                    }
                    List<ConditionData> conditionDataList = selectorConditionMapper
                            .selectByQuery(new SelectorConditionQuery(selectorDO.getId()))
                            .stream()
                            .map(ConditionTransfer.INSTANCE::mapToSelectorDO)
                            .collect(Collectors.toList());
                    zkClient.writeData(selectorRealPath, SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList));

                    final String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());

                    if (!zkClient.exists(ruleParentPath)) {
                        zkClient.createPersistent(ruleParentPath, true);
                    }
                    List<String> ruleZKs = zkClient.getChildren(ruleParentPath);
                    ruleMapper.selectByQuery(new RuleQuery(selectorDO.getId(), null))
                            .forEach(ruleDO -> {
                                if (CollectionUtils.isNotEmpty(ruleZKs)) {
                                    ruleZKs.remove(selectorDO.getId() + ZkPathConstants.SELECTOR_JOIN_RULE + ruleDO.getId());
                                }
                                String ruleRealPath = ZkPathConstants.buildRulePath(pluginDO.getName(), selectorDO.getId(), ruleDO.getId());
                                if (!zkClient.exists(ruleRealPath)) {
                                    zkClient.createPersistent(ruleRealPath, true);
                                }
                                List<ConditionData> conditionRuleDataList = ruleConditionMapper
                                        .selectByQuery(new RuleConditionQuery(ruleDO.getId()))
                                        .stream()
                                        .map(ConditionTransfer.INSTANCE::mapToRuleDO)
                                        .collect(Collectors.toList());
                                zkClient.writeData(ruleRealPath, RuleDO.transFrom(ruleDO, pluginDO.getName(), conditionRuleDataList));
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

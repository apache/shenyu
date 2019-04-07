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

package org.dromara.soul.web.cache;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.AppAuthZkDTO;
import org.dromara.soul.common.dto.zk.PluginZkDTO;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.dto.zk.SelectorZkDTO;
import org.dromara.soul.common.enums.PluginEnum;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * this cache data with zookeeper.
 *
 * @author xiaoyu
 */
@Component
public class ZookeeperCacheManager implements CommandLineRunner, DisposableBean {

    private static final Map<String, PluginZkDTO> PLUGIN_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<SelectorZkDTO>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<RuleZkDTO>> RULE_MAP = Maps.newConcurrentMap();

    private static final Map<String, AppAuthZkDTO> AUTH_MAP = Maps.newConcurrentMap();

    private final ZkClient zkClient;

    @Value("${soul.upstream.time:30}")
    private int upstreamTime;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param zkClient the zk client
     */
    @Autowired(required = false)
    public ZookeeperCacheManager(final ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    /**
     * acquire AppAuthZkDTO by appKey with AUTH_MAP container.
     *
     * @param appKey this is appKey.
     * @return AppAuthZkDTO {@linkplain AppAuthZkDTO}
     */
    public AppAuthZkDTO findAuthDTOByAppKey(final String appKey) {
        return AUTH_MAP.get(appKey);
    }

    /**
     * acquire PluginZkDTO by pluginName with PLUGIN_MAP container.
     *
     * @param pluginName this is plugin name.
     * @return PluginZkDTO {@linkplain  PluginZkDTO}
     */
    public PluginZkDTO findPluginByName(final String pluginName) {
        return PLUGIN_MAP.get(pluginName);
    }

    /**
     * acquire SelectorZkDTO list  by pluginName with  SELECTOR_MAP HashMap container.
     *
     * @param pluginName this is plugin name.
     * @return SelectorZkDTO list {@linkplain  SelectorZkDTO}
     */
    public List<SelectorZkDTO> findSelectorByPluginName(final String pluginName) {
        return SELECTOR_MAP.get(pluginName);
    }

    /**
     * acquire RuleZkDTO list by selectorId with  RULE_MAP HashMap container.
     *
     * @param selectorId this is selectorId.
     * @return RuleZkDTO list {@linkplain  RuleZkDTO}
     */
    public List<RuleZkDTO> findRuleBySelectorId(final String selectorId) {
        return RULE_MAP.get(selectorId);
    }

    @Override
    public void run(final String... args) {
        watcherData();
        watchAppAuth();
    }

    private void watcherData() {
        final String pluginParent = ZkPathConstants.PLUGIN_PARENT;
        if (!zkClient.exists(pluginParent)) {
            zkClient.createPersistent(pluginParent, true);
        }
        List<String> pluginZKs = zkClient.getChildren(ZkPathConstants.buildPluginParentPath());
        for (String pluginName : pluginZKs) {
            loadPlugin(pluginName);
        }
        zkClient.subscribeChildChanges(pluginParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String pluginName : currentChildren) {
                    loadPlugin(pluginName);
                }
            }
        });
    }

    private void loadPlugin(final String pluginName) {
        watcherPlugin(pluginName);
        watcherSelector(pluginName);
        watcherRule(pluginName);
    }

    private void watcherPlugin(final String pluginName) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginName);
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        PluginZkDTO data = zkClient.readData(pluginPath);
        Optional.ofNullable(data).ifPresent(d -> PLUGIN_MAP.put(pluginName, data));
        zkClient.subscribeDataChanges(pluginPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data)
                        .ifPresent(o -> {
                            PluginZkDTO dto = (PluginZkDTO) o;
                            PLUGIN_MAP.put(dto.getName(), dto);
                        });
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                PLUGIN_MAP.remove(pluginName);
            }
        });
    }

    private void watcherSelector(final String pluginName) {
        String selectorParentPath =
                ZkPathConstants.buildSelectorParentPath(pluginName);

        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }
        final List<String> childrenList = zkClient.getChildren(selectorParentPath);

        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(selectorParentPath, children);
                setSelectorData(realPath);
                subscribeSelectorDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(selectorParentPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(addPath -> {
                    final String realPath = buildRealPath(parentPath, addPath);
                    setSelectorData(realPath);
                    return realPath;
                }).forEach(this::subscribeSelectorDataChanges);

            }
        });
    }

    private void watcherRule(final String pluginName) {
        final String ruleParent = ZkPathConstants.buildRuleParentPath(pluginName);
        if (!zkClient.exists(ruleParent)) {
            zkClient.createPersistent(ruleParent, true);
        }
        final List<String> childrenList = zkClient.getChildren(ruleParent);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(ruleParent, children);
                setRuleData(realPath);
                subscribeRuleDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(ruleParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                //获取新增的节点数据，并对该节点进行订阅
                addSubscribePath.stream().map(addPath -> {
                    final String realPath = buildRealPath(parentPath, addPath);
                    setRuleData(realPath);
                    return realPath;
                })
                        .forEach(this::subscribeRuleDataChanges);
            }
        });
    }

    /**
     * set  SelectorMap by key.
     *
     * @param key           SELECTOR_MAP key.
     * @param selectorZkDTO data.
     */
    private void setSelectorMapByKey(final String key, final SelectorZkDTO selectorZkDTO) {
        Optional.ofNullable(key)
                .ifPresent(k -> {
                    if (selectorZkDTO.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
                        UpstreamCacheManager.submit(selectorZkDTO);
                    }
                    if (SELECTOR_MAP.containsKey(k)) {
                        final List<SelectorZkDTO> selectorZkDTOList = SELECTOR_MAP.get(key);
                        final List<SelectorZkDTO> resultList = selectorZkDTOList.stream()
                                .filter(r -> !r.getId()
                                        .equals(selectorZkDTO.getId()))
                                .collect(Collectors.toList());
                        resultList.add(selectorZkDTO);
                        final List<SelectorZkDTO> collect = resultList.stream()
                                .sorted(Comparator.comparing(SelectorZkDTO::getSort))
                                .collect(Collectors.toList());
                        SELECTOR_MAP.put(key, collect);
                    } else {
                        SELECTOR_MAP.put(key, Lists.newArrayList(selectorZkDTO));
                    }
                });
    }

    private void subscribeSelectorDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data).ifPresent(d -> selectorDataChange((SelectorZkDTO) d));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                //规定路径 key-id key为selectorId, id为规则id
                final String id = dataPath.substring(dataPath.lastIndexOf("/") + 1);
                final String str = dataPath.substring(ZkPathConstants.SELECTOR_PARENT.length());
                final String key = str.substring(1, str.length() - id.length() - 1);
                Optional.of(key).ifPresent(k -> {
                    final List<SelectorZkDTO> selectorZkDTOList = SELECTOR_MAP.get(k);
                    selectorZkDTOList.removeIf(e -> e.getId().equals(id));
                });
            }
        });
    }

    private void setSelectorData(final String realPath) {
        final SelectorZkDTO selectorZkDTO = zkClient.readData(realPath);
        Optional.ofNullable(selectorZkDTO)
                .ifPresent(dto -> {
                    final String key = dto.getPluginName();
                    setSelectorMapByKey(key, dto);
                });
    }

    private void setRuleData(final String rulePath) {
        final RuleZkDTO ruleZkDTO = zkClient.readData(rulePath);
        Optional.ofNullable(ruleZkDTO)
                .ifPresent(dto -> {
                    String key = dto.getSelectorId();
                    setRuleMapByKey(key, ruleZkDTO);
                });
    }

    private void setRuleMapByKey(final String key, final RuleZkDTO ruleZkDTO) {
        Optional.ofNullable(key)
                .ifPresent(k -> {
                    if (RULE_MAP.containsKey(k)) {
                        final List<RuleZkDTO> ruleZkDTOList = RULE_MAP.get(key);
                        final List<RuleZkDTO> resultList = ruleZkDTOList.stream()
                                .filter(r -> !r.getId()
                                        .equals(ruleZkDTO.getId()))
                                .collect(Collectors.toList());
                        resultList.add(ruleZkDTO);
                        final List<RuleZkDTO> collect = resultList.stream()
                                .sorted(Comparator.comparing(RuleZkDTO::getSort))
                                .collect(Collectors.toList());
                        RULE_MAP.put(key, collect);

                    } else {
                        RULE_MAP.put(key, Lists.newArrayList(ruleZkDTO));
                    }
                });
    }

    private void subscribeRuleDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data).ifPresent(d -> ruleDataChange((RuleZkDTO) d));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                //规定路径 key-id key为selectorId, id为规则id
                final List<String> list = Splitter.on(ZkPathConstants.SELECTOR_JOIN_RULE)
                        .splitToList(dataPath.substring(dataPath.lastIndexOf("/") + 1));
                final String key = list.get(0);
                final String id = list.get(1);
                UpstreamCacheManager.removeByKey(id);
                Optional.ofNullable(key).ifPresent(k -> {
                    final List<RuleZkDTO> ruleZkDTOList = RULE_MAP.get(k);
                    ruleZkDTOList.removeIf(e -> e.getId().equals(id));
                });
            }
        });
    }

    private void selectorDataChange(final SelectorZkDTO dto) {
        if (dto.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
            UpstreamCacheManager.submit(dto);
        }
        final String key = dto.getPluginName();
        final List<SelectorZkDTO> selectorZkDTOList = SELECTOR_MAP.get(key);
        if (CollectionUtils.isNotEmpty(selectorZkDTOList)) {
            final List<SelectorZkDTO> resultList =
                    selectorZkDTOList.stream().filter(r -> !r.getId().equals(dto.getId())).collect(Collectors.toList());
            resultList.add(dto);
            final List<SelectorZkDTO> collect = resultList.stream()
                    .sorted(Comparator.comparing(SelectorZkDTO::getSort))
                    .collect(Collectors.toList());
            SELECTOR_MAP.put(key, collect);
        } else {
            SELECTOR_MAP.put(key, Lists.newArrayList(dto));
        }
    }

    private void ruleDataChange(final RuleZkDTO dto) {
        final String key = dto.getSelectorId();
        final List<RuleZkDTO> ruleZkDTOList = RULE_MAP.get(key);
        if (CollectionUtils.isNotEmpty(ruleZkDTOList)) {
            final List<RuleZkDTO> resultList = ruleZkDTOList.stream()
                    .filter(r -> !r.getId()
                            .equals(dto.getId())).collect(Collectors.toList());
            resultList.add(dto);
            final List<RuleZkDTO> collect = resultList.stream()
                    .sorted(Comparator.comparing(RuleZkDTO::getSort))
                    .collect(Collectors.toList());
            RULE_MAP.put(key, collect);
        } else {
            RULE_MAP.put(key, Lists.newArrayList(dto));
        }
    }

    private void watchAppAuth() {
        final String appAuthParent = ZkPathConstants.APP_AUTH_PARENT;
        if (!zkClient.exists(appAuthParent)) {
            zkClient.createPersistent(appAuthParent, true);
        }
        final List<String> childrenList = zkClient.getChildren(appAuthParent);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(appAuthParent, children);
                setAuthData(realPath);
                subscribeAppAuthDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(appAuthParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(children -> {
                    final String realPath = buildRealPath(parentPath, children);
                    setAuthData(realPath);
                    return realPath;
                }).forEach(this::subscribeAppAuthDataChanges);
            }
        });
    }

    private void setAuthData(final String realPath) {
        final AppAuthZkDTO appAuthZkDTO = zkClient.readData(realPath);
        Optional.ofNullable(appAuthZkDTO)
                .ifPresent(dto -> AUTH_MAP.put(dto.getAppKey(), dto));
    }

    private void subscribeAppAuthDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data)
                        .ifPresent(o -> AUTH_MAP.put(((AppAuthZkDTO) o).getAppKey(), (AppAuthZkDTO) o));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                final String key = dataPath.substring(ZkPathConstants.APP_AUTH_PARENT.length() + 1);
                AUTH_MAP.remove(key);
            }
        });
    }

    private List<String> addSubscribePath(final List<String> alreadyChildren, final List<String> currentChildren) {
        if (CollectionUtils.isEmpty(alreadyChildren)) {
            return currentChildren;
        }
        return currentChildren.stream().filter(current -> alreadyChildren.stream().noneMatch(current::equals)).collect(Collectors.toList());
    }

    private String buildRealPath(final String parent, final String children) {
        return parent + "/" + children;
    }

    @Override
    public void destroy() {
        zkClient.close();
    }
}

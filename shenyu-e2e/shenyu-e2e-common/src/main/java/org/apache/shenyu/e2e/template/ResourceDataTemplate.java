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

package org.apache.shenyu.e2e.template;

import com.google.common.base.Strings;
import groovyjarjarantlr4.v4.runtime.misc.NotNull;
import groovyjarjarantlr4.v4.runtime.misc.Nullable;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.SelectorType;
import org.apache.shenyu.e2e.model.data.BindingData;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.data.RuleData;
import org.apache.shenyu.e2e.model.data.SelectorData;
import org.apache.shenyu.e2e.model.handle.DivideRuleHandle;
import org.apache.shenyu.e2e.model.handle.Upstreams;

import jakarta.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.e2e.constant.Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;

/**
 * Templates for various entity classes.
 */
public class ResourceDataTemplate {

    /**
     * Build new SelectorBuilder.
     *
     * @param name   name
     * @param plugin plugin
     * @return SelectorData.Builder
     */
    public static SelectorData.SelectorDataBuilder newSelectorBuilder(final @NotNull String name, final Plugin plugin) {
        return SelectorData.builder()
                .name(name)
                .plugin(plugin)
                .type(SelectorType.CUSTOM)
                .matchMode(MatchMode.AND)
                .continued(true)
                .logged(true)
                .enabled(true)
                .matchRestful(false)
                .sort(1);
    }

    /**
     * newBindingData.
     *
     * @param selectorName selectorName
     * @param pluginName   pluginName
     * @param url          url
     * @return BindingData
     */
    public static BindingData newBindingData(final String selectorName, final String pluginName, final String url) {
        BindingData bindingData = new BindingData();
        bindingData.setName(selectorName);
        bindingData.setPluginName(pluginName);
        bindingData.setType("http");
        BindingData.Discovery discovery = new BindingData.Discovery();
        discovery.setDiscoveryType("local");
        discovery.setProps("{}");
        bindingData.setDiscovery(discovery);
        BindingData.DiscoveryUpstream discoveryUpstream = new BindingData.DiscoveryUpstream();
        discoveryUpstream.setUrl(url);
        discoveryUpstream.setProtocol("http://");
        discoveryUpstream.setStatus(0);
        discoveryUpstream.setWeight(50);
        discoveryUpstream.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        bindingData.setDiscoveryUpstreams(Collections.singletonList(discoveryUpstream));
        return bindingData;
    }

    /**
     * Build new RuleBuilder.
     *
     * @param name name
     * @return RuleData.Builder
     */
    public static RuleData.RuleDataBuilder newRuleBuilder(final @Nonnull String name) {
        return newRuleBuilder(name, null);
    }

    /**
     * new rule builder.
     *
     * @param name       name
     * @param selectorId selectorId
     * @return RuleData.RuleDataBuilder
     */
    public static RuleData.RuleDataBuilder newRuleBuilder(final @Nonnull String name, final String selectorId) {
        return RuleData.builder()
                .name(name)
                .matchMode(MatchMode.AND)
                .selectorId(selectorId)
                .enabled(true)
                .logged(true)
                .matchRestful(false)
                .sort(1);
    }

    /**
     * Build new DivideRuleHandle.
     *
     * @return DivideRuleHandle
     */
    public static DivideRuleHandle newDivideRuleHandle() {
        return DivideRuleHandle.builder()
                .loadBalance("hash")
                .retryStrategy("current")
                .retry(1)
                .timeout(3000)
                .headerMaxSize(10240)
                .requestMaxSize(10240)
                .build();
    }

    /**
     * Build new Condition.
     *
     * @param type  type
     * @param opt   opt
     * @param value value
     * @return Condition
     */
    public static Condition newCondition(final Condition.ParamType type, final Condition.Operator opt, final String value) {
        return newCondition(type, opt, null, value);
    }

    /**
     * new condition.
     *
     * @param type  type
     * @param opt   opt
     * @param key   key
     * @param value value
     * @return Condition
     */
    public static Condition newCondition(final Condition.ParamType type, final Condition.Operator opt, final @Nullable String key, final String value) {
        return Condition.builder()
                .paramType(type)
                .operator(opt)
                .paramName(Strings.isNullOrEmpty(key) ? "/" : key)
                .paramValue(value)
                .build();
    }

    /**
     * new conditions.
     *
     * @param type  type
     * @param opt   opt
     * @param value value
     * @return List
     */
    public static List<Condition> newConditions(final Condition.ParamType type, final Condition.Operator opt, final String value) {
        ArrayList<Condition> list = new ArrayList<>();
        list.add(newCondition(type, opt, value));
        return list;
    }

    /**
     * Build new Upstream.
     *
     * @param url url
     * @return Upstream
     */
    public static Upstreams.Upstream newUpstream(final String url) {
        return Upstreams.Upstream.builder()
                .upstreamUrl(url)
                .build();
    }

    /**
     * new upstream builder.
     *
     * @param url url
     * @return Upstreams
     */
    public static Upstreams newUpstreamsBuilder(final String url) {
        return Upstreams.builder().add(newUpstream(url)).build();
    }
}

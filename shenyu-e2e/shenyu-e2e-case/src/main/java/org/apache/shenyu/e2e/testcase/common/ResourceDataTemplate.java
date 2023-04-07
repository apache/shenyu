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

package org.apache.shenyu.e2e.testcase.common;

import com.google.common.base.Strings;
import org.apache.shenyu.e2e.client.admin.model.MatchMode;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.SelectorType;
import org.apache.shenyu.e2e.client.admin.model.data.Condition;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.Operator;
import org.apache.shenyu.e2e.client.admin.model.data.Condition.ParamType;
import org.apache.shenyu.e2e.client.admin.model.data.RuleData;
import org.apache.shenyu.e2e.client.admin.model.data.SelectorData;
import org.apache.shenyu.e2e.client.admin.model.handle.DivideRuleHandle;
import org.apache.shenyu.e2e.client.admin.model.handle.Upstreams;
import org.apache.shenyu.e2e.client.admin.model.handle.Upstreams.Upstream;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

// TODO replace it by introducing Auto-Value-Annotations?
public class ResourceDataTemplate {
    
    public static SelectorData.SelectorDataBuilder newSelectorBuilder(@NotNull String name, Plugin plugin) {
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
    
    public static RuleData.RuleDataBuilder newRuleBuilder(@Nonnull String name) {
        return newRuleBuilder(name, null);
    }
    
    public static RuleData.RuleDataBuilder newRuleBuilder(@Nonnull String name, String selectorId) {
        return RuleData.builder()
                .name(name)
                .matchMode(MatchMode.AND)
                .selectorId(selectorId)
                .enabled(true)
                .logged(true)
                .matchRestful(false)
                .sort(1);
    }
    
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
    
    public static Condition newCondition(ParamType type, Operator opt, String value) {
        return newCondition(type, opt, null, value);
    }
    
    public static Condition newCondition(ParamType type, Operator opt, @Nullable String key, String value) {
        return Condition.builder()
                .paramType(type)
                .operator(opt)
                .paramName(Strings.isNullOrEmpty(key) ? "/" : key)
                .paramValue(value)
                .build();
    }
    
    public static List<Condition> newConditions(ParamType type, Operator opt, String value) {
        ArrayList<Condition> list = new ArrayList<>();
        list.add(newCondition(type, opt, value));
        return list;
    }
    
    public static Upstream newUpstream(String url) {
        return Upstream.builder()
                .upstreamUrl(url)
                .build();
    }
    
    public static Upstreams newUpstreamsBuilder(String url) {
        return Upstreams.builder().add(newUpstream(url)).build();
    }
}

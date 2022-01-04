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

package org.apache.shenyu.agent.plugin.tracing.common.definition;

import net.bytebuddy.matcher.ElementMatchers;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.entity.PointCutConfig;
import org.apache.shenyu.agent.api.entity.PointCutConfig.Point;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint.JoinPointBuilder;
import org.apache.shenyu.agent.api.spi.AbstractAgentPluginDefinition;
import org.apache.shenyu.agent.core.enums.PointType;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.apache.shenyu.agent.core.utils.ShenyuAgentConfigUtils;
import org.apache.shenyu.agent.core.yaml.ShenyuYamlEngine;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Join
public final class TracingAgentPluginDefinition extends AbstractAgentPluginDefinition {
    
    private static final Logger LOG = LoggerFactory.getLogger(TracingAgentPluginDefinition.class);
    
    @Override
    protected Collection<JoinPointBuilder> joinPointBuilder() {
        PointCutConfig config = null;
        try {
            config = ShenyuYamlEngine.unmarshal(ShenyuAgentLocator.locatorConf("tracing-point.yaml"), PointCutConfig.class);
        } catch (IOException e) {
            LOG.error("Exception loader tracing point config is", e);
        } 
        if (Objects.isNull(config) || config.getPointCuts().isEmpty()) {
            return Collections.emptyList();
        }
        return config.getPointCuts().stream()
                .filter(pointCut -> StringUtils.isNotEmpty(pointCut.getTargetClass()) 
                        && !pointCut.getPoints().isEmpty() && !pointCut.getHandlers().isEmpty())
                .map(pointCut -> {
                    JoinPointBuilder builder = ShenyuAgentJoinPoint.interceptClass(pointCut.getTargetClass());
                    Set<String> supports = ShenyuAgentConfigUtils.getSupports();
                    List<String> handlers = pointCut.getHandlers().entrySet().stream()
                            .filter(entry -> supports.contains(entry.getKey()))
                            .flatMap(entry -> entry.getValue().stream())
                            .collect(Collectors.toList());
                    String[] instanceMethods = pointCut
                            .getPoints()
                            .stream()
                            .filter(point -> PointType.INSTANCE_METHOD.getName().equals(point.getType()))
                            .map(Point::getName)
                            .toArray(String[]::new);
                    if (instanceMethods.length > 0) {
                        builder.aroundInstanceMethod(ElementMatchers.namedOneOf(instanceMethods)).handlers(handlers).build();
                    }
                    String[] staticMethods = pointCut
                            .getPoints()
                            .stream()
                            .filter(point -> PointType.STATIC_METHOD.getName().equals(point.getType()))
                            .map(Point::getName)
                            .toArray(String[]::new);
                    if (staticMethods.length > 0) {
                        builder.aroundStaticMethod(ElementMatchers.namedOneOf(staticMethods)).handlers(handlers).build();
                    }
                    String[] constructorPoints = pointCut
                            .getPoints()
                            .stream()
                            .filter(point -> PointType.CONSTRUCTOR.getName().equals(point.getType()))
                            .map(Point::getName)
                            .toArray(String[]::new);
                    if (constructorPoints.length > 0) {
                        builder.onConstructor(ElementMatchers.namedOneOf(constructorPoints)).handlers(handlers).build();
                    }
                    return builder;
                })
                .collect(Collectors.toList());
    }
}

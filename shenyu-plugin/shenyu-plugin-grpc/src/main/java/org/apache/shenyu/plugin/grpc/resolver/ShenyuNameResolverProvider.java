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

package org.apache.shenyu.plugin.grpc.resolver;

import io.grpc.NameResolver;
import io.grpc.NameResolverProvider;
import io.grpc.internal.GrpcUtil;
import org.apache.shenyu.common.enums.PluginEnum;

import java.net.URI;

/**
 * ShenyuNameResolverProvider.
 */
public class ShenyuNameResolverProvider extends NameResolverProvider {

    @Override
    public NameResolver newNameResolver(final URI targetUri, final NameResolver.Args args) {
        return new ShenyuNameResolver(targetUri.getPath(), args, GrpcUtil.SHARED_CHANNEL_EXECUTOR);
    }

    @Override
    protected boolean isAvailable() {
        return true;
    }

    @Override
    protected int priority() {
        return 6;
    }

    @Override
    public String getDefaultScheme() {
        return PluginEnum.GRPC.getName();
    }
}

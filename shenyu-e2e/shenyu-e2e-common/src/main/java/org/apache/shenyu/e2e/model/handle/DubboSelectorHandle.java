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

package org.apache.shenyu.e2e.model.handle;

import java.util.List;

public class DubboSelectorHandle implements PluginHandle {

    private List<DubboHandler> handle;

    public DubboSelectorHandle() {
    }

    public DubboSelectorHandle(Builder builder) {
        this.handle = builder.handle;
    }

    public static Builder builder() {
        return new Builder();
    }

    public List<DubboHandler> getHandle() {
        return handle;
    }

    public void setHandle(List<DubboHandler> handle) {
        this.handle = handle;
    }

    public static final class Builder {

        private List<DubboHandler> handle;

        public Builder() {
        }

        public DubboSelectorHandle build() {
            return new DubboSelectorHandle(this);
        }

        public Builder handle(List<DubboHandler> handle) {
            this.handle = handle;
            return this;
        }
    }
}

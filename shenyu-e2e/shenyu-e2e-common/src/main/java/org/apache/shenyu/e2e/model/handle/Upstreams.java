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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.model.handle.Upstreams.Serializer;

import java.io.IOException;
import java.util.List;

/**
 * upstream list.
 */
@JsonSerialize(using = Serializer.class)
public class Upstreams implements PluginHandle {
    
    private List<Upstream> upstreams;
    
    Upstreams(final List<Upstream> upstreams) {
        this.upstreams = upstreams;
    }

    /**
     * get upstreams.
     *
     * @return upstreams
     */
    public List<Upstream> getUpstreams() {
        return this.upstreams;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * class upstream.
     */
    public static final class Upstream {

        private String upstreamUrl;

        /**
         * builder constructor.
         *
         * @param builder builder
         */
        private Upstream(final Builder builder) {
            this.upstreamUrl = builder.upstreamUrl;
        }

        /**
         * class builder.
         *
         * @return Builder
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * get upstreamUrl.
         *
         * @return upstreamUrl
         */
        public String getUpstreamUrl() {
            return upstreamUrl;
        }

        /**
         * set upstreamUrl.
         *
         * @param upstreamUrl upstreamUrl
         */
        public void setUpstreamUrl(final String upstreamUrl) {
            this.upstreamUrl = upstreamUrl;
        }

        /**
         * class builder.
         */
        public static final class Builder {

            private String upstreamUrl;

            /**
             * no args constructor.
             */
            private Builder() {
            }

            /**
             * build new Object.
             *
             * @return Upstream
             */
            public Upstream build() {
                return new Upstream(this);
            }

            /**
             * build upstreamUrl.
             *
             * @param upstreamUrl upstreamUrl
             * @return this
             */
            public Builder upstreamUrl(final String upstreamUrl) {
                this.upstreamUrl = upstreamUrl;
                return this;
            }
        }
    }

    /**
     * class Builder.
     */
    public static final class Builder {

        private final List<Upstream> upstreams = Lists.newArrayList();

        /**
         * add upstream.
         *
         * @param upstream upstream
         * @return this
         */
        public Builder add(final Upstream upstream) {
            upstreams.add(upstream);
            return this;
        }
        
        /**
         * build.
         * @return Upstreams
         */
        public Upstreams build() {
            return new Upstreams(this.upstreams);
        }
    }
    
    static class Serializer extends JsonSerializer<Upstreams> {
        private final ObjectMapper mapper = new ObjectMapper();
        
        @Override
        public void serialize(final Upstreams upstreams, final JsonGenerator jsonGenerator, final SerializerProvider serializerProvider) throws IOException {
            jsonGenerator.writeRaw(mapper.writer().writeValueAsString(upstreams.upstreams));
        }
    }
}

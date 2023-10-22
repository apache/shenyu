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
import org.assertj.core.util.Lists;

import java.io.IOException;
import java.util.List;

@JsonSerialize(using = WebSocketSelectorHandles.Serializer.class)
public class WebSocketSelectorHandles implements PluginHandle {

    private List<WebSocketHandler> handle;

    public WebSocketSelectorHandles() {
    }

    public WebSocketSelectorHandles(final Builder builder) {
        this.handle = builder.handle;
    }

    /**
     * builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get handle.
     *
     * @return handle.
     */
    public List<WebSocketHandler> getHandle() {
        return handle;
    }

    /**
     * set handle.
     *
     * @param handle handle
     */
    public void setHandle(final List<WebSocketHandler> handle) {
        this.handle = handle;
    }

    public static final class Builder {

        private List<WebSocketHandler> handle = Lists.newArrayList();

        public Builder() {
        }

        public Builder add(final WebSocketHandler webSocketHandler) {
            handle.add(webSocketHandler);
            return this;
        }

        /**
         * build DubboSelectorHandle.
         *
         * @return DubboSelectorHandle
         */
        public WebSocketSelectorHandles build() {
            return new WebSocketSelectorHandles(this);
        }

        /**
         * build handle.
         *
         * @param handle handle
         * @return Builder
         */
        public Builder handle(final List<WebSocketHandler> handle) {
            this.handle = handle;
            return this;
        }
    }

    public static final class WebSocketHandler {
        /**
         * upstreamHost.
         */
        private String host;

        private String protocol;

        private String url;

        private boolean status;

        /**
         * startup time.
         */
        private long timestamp;

        private int warmup;

        /**
         * weight.
         */
        private int weight;

        public WebSocketHandler() {
        }

        public WebSocketHandler(final Builder builder) {
            this.host = builder.host;
            this.protocol = builder.protocol;
            this.url = builder.url;
            this.status = builder.status;
            this.timestamp = builder.timestamp;
            this.warmup = builder.warmup;
            this.weight = builder.weight;
        }

        /**
         * builder.
         *
         * @return Builder
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * get upstreamHost.
         *
         * @return upstreamHost
         */
        public String getHost() {
            return host;
        }

        /**
         * set upstreamHost.
         *
         * @param host upstreamHost
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * get protocol.
         * @return protocol
         */
        public String getProtocol() {
            return protocol;
        }

        /**
         * set protocol.
         *
         * @param protocol protocol
         */
        public void setProtocol(final String protocol) {
            this.protocol = protocol;
        }

        /**
         * get upstreamUrl.
         *
         * @return upstreamUrl
         */
        public String getUrl() {
            return url;
        }

        /**
         * set upstreamUrl.
         *
         * @param url upstreamUrl
         */
        public void setUrl(final String url) {
            this.url = url;
        }

        /**
         * is status.
         *
         * @return status
         */
        public boolean isStatus() {
            return status;
        }

        /**
         * set status.
         * @param status status
         */
        public void setStatus(final boolean status) {
            this.status = status;
        }

        /**
         * get timestamp.
         *
         * @return timestamp
         */
        public long getTimestamp() {
            return timestamp;
        }

        /**
         * set timestamp.
         *
         * @param timestamp timestamp
         */
        public void setTimestamp(final long timestamp) {
            this.timestamp = timestamp;
        }

        /**
         * get warmup.
         *
         * @return warmup
         */
        public long getWarmup() {
            return warmup;
        }

        /**
         * set warmup.
         *
         * @param warmup warmup
         */
        public void setWarmup(final int warmup) {
            this.warmup = warmup;
        }

        /**
         * get weight.
         *
         * @return weight
         */
        public int getWeight() {
            return weight;
        }

        /**
         * set weight.
         *
         * @param weight weight
         */
        public void setWeight(final int weight) {
            this.weight = weight;
        }

        public static final class Builder {
            /**
             * upstreamHost.
             */
            private String host;

            private String protocol;

            private String url;

            private boolean status;

            /**
             * startup time.
             */
            private long timestamp;

            private int warmup;

            /**
             * weight.
             */
            private int weight;

            private Builder() {
            }

            /**
             * build new Object.
             *
             * @return DubboHandler
             */
            public WebSocketHandler build() {
                return new WebSocketHandler(this);
            }

            /**
             * build upstreamHost.
             *
             * @param upstreamHost upstreamHost
             * @return upstreamHost
             */
            public Builder upstreamHost(final String upstreamHost) {
                this.host = upstreamHost;
                return this;
            }

            /**
             * build protocol.
             *
             * @param protocol protocol
             * @return protocol
             */
            public Builder protocol(final String protocol) {
                this.protocol = protocol;
                return this;
            }

            /**
             * build upstreamUrl.
             *
             * @param upstreamUrl upstreamUrl
             * @return upstreamUrl
             */
            public Builder upstreamUrl(final String upstreamUrl) {
                this.url = upstreamUrl;
                return this;
            }

            /**
             * build status.
             *
             * @param status status
             * @return status
             */
            public Builder status(final boolean status) {
                this.status = status;
                return this;
            }

            /**
             * build timestamp.
             *
             * @param timestamp timestamp
             * @return timestamp
             */
            public Builder timestamp(final long timestamp) {
                this.timestamp = timestamp;
                return this;
            }

            /**
             * build warmup.
             *
             * @param warmup warmup
             * @return warmup
             */
            public Builder warmup(final int warmup) {
                this.warmup = warmup;
                return this;
            }

            /**
             * build weight.
             *
             * @param weight weight
             * @return weight
             */
            public Builder weight(final int weight) {
                this.weight = weight;
                return this;
            }
        }
    }

    static class Serializer extends JsonSerializer<WebSocketSelectorHandles> {
        private final ObjectMapper mapper = new ObjectMapper();

        @Override
        public void serialize(WebSocketSelectorHandles webSocketSelectorHandles, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
            jsonGenerator.writeRaw(mapper.writer().writeValueAsString(webSocketSelectorHandles.handle));
        }
    }
}

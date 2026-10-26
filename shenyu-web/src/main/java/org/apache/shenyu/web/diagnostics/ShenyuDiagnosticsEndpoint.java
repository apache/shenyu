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

package org.apache.shenyu.web.diagnostics;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

/**
 * Gateway management endpoint that exports a bounded, redacted diagnostic bundle.
 *
 * <p>The endpoint is registered only when {@code shenyu.diagnostics.enabled} is {@code true}, so a
 * default deployment does not expose it. Collection runs on the bounded-elastic scheduler and is
 * assembled in memory; the download filename is generated from a timestamp so no caller-supplied
 * value can influence it. Exposing this path behind the deployment's own authentication is the
 * operator's responsibility, exactly like the other gateway actuator endpoints.</p>
 */
@RestController
@RequestMapping("/actuator/diagnostics")
@ConditionalOnProperty(name = "shenyu.diagnostics.enabled", havingValue = "true")
public class ShenyuDiagnosticsEndpoint {

    private final DiagnosticBundleService bundleService;

    /**
     * Instantiates a new diagnostics endpoint.
     *
     * @param bundleService the bundle service
     */
    public ShenyuDiagnosticsEndpoint(final DiagnosticBundleService bundleService) {
        this.bundleService = bundleService;
    }

    /**
     * Export the diagnostic bundle as a ZIP archive download.
     *
     * @return the archive bytes wrapped in an attachment response
     */
    @GetMapping(value = "/bundle", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public Mono<ResponseEntity<byte[]>> bundle() {
        return Mono.fromCallable(bundleService::bundle)
                .subscribeOn(Schedulers.boundedElastic())
                .map(this::toAttachment);
    }

    private ResponseEntity<byte[]> toAttachment(final byte[] body) {
        String filename = "shenyu-diagnostics-" + System.currentTimeMillis() + ".zip";
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .contentLength(body.length)
                .body(body);
    }
}

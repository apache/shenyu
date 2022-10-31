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

package org.apache.shenyu.web.controller;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;

/**
 * The type Meta data controller.
 */
@RestController
@RequestMapping(value = "/shenyu", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class LocalMetadataController {

    private static final Logger LOG = LoggerFactory.getLogger(LocalMetadataController.class);

    private final List<MetaDataSubscriber> subscribers;

    /**
     * Instantiates a new Local metadata controller.
     *
     * @param subscribers the subscribers
     */
    public LocalMetadataController(final ObjectProvider<List<MetaDataSubscriber>> subscribers) {
        this.subscribers = subscribers.getIfAvailable(ArrayList::new);
    }

    /**
     * Clean mono.
     *
     * @param id the rpc id
     * @param rpcType the rpc type
     * @param path the path
     * @return the mono
     */
    @GetMapping("/meta/delete")
    public Mono<String> clean(@RequestParam("id") final String id,
                              @RequestParam("rpcType") final String rpcType,
                              @RequestParam("path") final String path) {
        if (CollectionUtils.isEmpty(subscribers)) {
            return Mono.just(Constants.SUCCESS);
        }
        LOG.info("delete apache shenyu local meta data");
        MetaData metaData = MetaData.builder().id(id).rpcType(rpcType).path(path).build();
        subscribers.forEach(metaDataSubscriber -> metaDataSubscriber.unSubscribe(metaData));
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Save or update mono.
     *
     * @param metaData the meta data
     * @return the mono
     */
    @PostMapping("/meta/saveOrUpdate")
    public Mono<String> saveOrUpdate(@RequestBody final MetaData metaData) {
        if (CollectionUtils.isEmpty(subscribers)) {
            return Mono.just(Constants.SUCCESS);
        }
        LOG.info("saveOrUpdate apache shenyu local meta data");
        subscribers.forEach(metaDataSubscriber -> metaDataSubscriber.onSubscribe(metaData));
        return Mono.just(Constants.SUCCESS);
    }
}

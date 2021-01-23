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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The type Soul client controller.
 *
 * @author xiaoyu
 */
@RestController
@RequestMapping("/soul-client")
public class SoulClientController {

    private final SoulClientRegisterService soulClientRegisterService;

    /**
     * Instantiates a new Soul client controller.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public SoulClientController(final SoulClientRegisterService soulClientRegisterService) {
        this.soulClientRegisterService = soulClientRegisterService;
    }

    /**
     * Register spring mvc string.
     *
     * @param springMvcRegisterDTO the spring mvc register dto
     * @return the string
     */
    @PostMapping("/springmvc-register")
    public String registerSpringMvc(@RequestBody final SpringMvcRegisterDTO springMvcRegisterDTO) {
        return soulClientRegisterService.registerSpringMvc(springMvcRegisterDTO);
    }

    /**
     * Register spring cloud string.
     *
     * @param springCloudRegisterDTO the spring cloud register dto
     * @return the string
     */
    @PostMapping("/springcloud-register")
    public String registerSpringCloud(@RequestBody final SpringCloudRegisterDTO springCloudRegisterDTO) {
        return soulClientRegisterService.registerSpringCloud(springCloudRegisterDTO);
    }


    /**
     * Register dubbo string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    @PostMapping("/dubbo-register")
    public String registerRpc(@RequestBody final MetaDataDTO metaDataDTO) {
        return soulClientRegisterService.registerDubbo(metaDataDTO);
    }

    /**
     * Register sofa string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    @PostMapping("/sofa-register")
    public String registerSofaRpc(@RequestBody final MetaDataDTO metaDataDTO) {
        return soulClientRegisterService.registerSofa(metaDataDTO);
    }

    /**
     * Register tars string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    @PostMapping("/tars-register")
    public String registerTarsRpc(@RequestBody final MetaDataDTO metaDataDTO) {
        return soulClientRegisterService.registerTars(metaDataDTO);
    }
}

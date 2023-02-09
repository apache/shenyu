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

package org.apache.shenyu.admin.service.impl;

import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ProxyGatewayDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.SandboxService;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.admin.utils.ShenyuSignatureUtils;
import org.apache.shenyu.admin.utils.UploadUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.util.UriUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * sandbox service.
 */
@Service
public class SandboxServiceImpl implements SandboxService {

    private static final Logger LOG = LoggerFactory.getLogger(SandboxServiceImpl.class);

    private static final HttpUtils HTTP_UTILS = new HttpUtils();

    private final AppAuthService appAuthService;

    public SandboxServiceImpl(final AppAuthService appAuthService) {
        this.appAuthService = appAuthService;
    }

    @Override
    public void requestProxyGateway(final ProxyGatewayDTO proxyGatewayDTO, final HttpServletRequest request, final HttpServletResponse response) throws IOException {
        // Public request headers.
        Map<String, String> header = this.buildReqHeaders(proxyGatewayDTO);

        String appKey = proxyGatewayDTO.getAppKey();
        UriComponents uriComponents = UriComponentsBuilder.fromHttpUrl(proxyGatewayDTO.getRequestUrl()).build();
        String signContent = null;
        String sign = null;
        if (StringUtils.isNotEmpty(appKey)) {
            String timestamp = String.valueOf(Instant.now().toEpochMilli());
            String secureKey = getSecureKey(appKey);
            Assert.notBlack(secureKey, Constants.SIGN_APP_KEY_IS_NOT_EXIST);
            signContent = ShenyuSignatureUtils.getSignContent(secureKey, timestamp, uriComponents.getPath());
            sign = ShenyuSignatureUtils.generateSign(signContent);
            header.put("timestamp", timestamp);
            header.put("appKey", appKey);
            header.put("sign", sign);
            header.put("version", ShenyuSignatureUtils.VERSION);
        }

        // Public request parameters.
        Map<String, Object> reqParams = this.buildReqBizParams(proxyGatewayDTO);
        List<HttpUtils.UploadFile> files = this.uploadFiles(request);
        Response resp = HTTP_UTILS.requestCall(uriComponents.toUriString(), reqParams, header, HttpUtils.HTTPMethod.fromValue(proxyGatewayDTO.getHttpMethod()), files);
        ResponseBody body = resp.body();

        if (Objects.isNull(body)) {
            return;
        }
        if (StringUtils.isNotEmpty(appKey)) {
            response.addHeader("sandbox-beforesign", UriUtils.encode(signContent, StandardCharsets.UTF_8));
            response.addHeader("sandbox-sign", UriUtils.encode(sign, StandardCharsets.UTF_8));
        }

        IOUtils.copy(body.byteStream(), response.getOutputStream());
        response.flushBuffer();
    }

    private Map<String, String> buildReqHeaders(final ProxyGatewayDTO proxyGatewayDTO) {
        Map<String, String> reqHeaders = new HashMap<>();
        try {
            String reqJson = JsonUtils.toJson(proxyGatewayDTO.getHeaders());
            Map<String, String> reqMap = JsonUtils.jsonToMap(reqJson, String.class);
            LOG.info("Sandbox Request Headers. toMap={}", JsonUtils.toJson(reqMap));
            reqHeaders.putAll(reqMap);
        } catch (Exception e) {
            LOG.error("proxyGateway JsonUtils.toMap error={}", e.getMessage());
        }
        return reqHeaders;
    }

    private Map<String, Object> buildReqBizParams(final ProxyGatewayDTO proxyGatewayDTO) {
        Map<String, Object> reqParams = new HashMap<>();
        try {
            Map<String, Object> reqMap = JsonUtils.toMap(proxyGatewayDTO.getBizParam());
            LOG.info("sandbox Request Params. toMap={}", JsonUtils.toJson(reqMap));
            reqParams.putAll(reqMap);
        } catch (Exception e) {
            LOG.error("proxyGateway JsonUtils.toMap error={}", e.getMessage());
        }
        return reqParams;
    }

    private String getSecureKey(final String appKey) {
        AppAuthDO appAuthDO = appAuthService.findByAppKey(appKey);
        return Objects.nonNull(appAuthDO) ? appAuthDO.getAppSecret() : null;
    }

    private List<HttpUtils.UploadFile> uploadFiles(final HttpServletRequest request) {
        Collection<MultipartFile> uploadFiles = UploadUtils.getUploadFiles(request);
        return uploadFiles.stream()
            .map(multipartFile -> {
                try {
                    return new HttpUtils.UploadFile(multipartFile.getName(), multipartFile.getOriginalFilename(), multipartFile.getBytes());
                } catch (IOException e) {
                    LOG.error("upload file fail", e);
                    return null;
                }
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

}

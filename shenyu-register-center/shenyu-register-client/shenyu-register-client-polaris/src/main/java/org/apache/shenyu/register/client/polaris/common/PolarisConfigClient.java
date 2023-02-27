package org.apache.shenyu.register.client.polaris.common;

import org.apache.shenyu.register.client.polaris.constant.ParameterKeyConstant;
import org.apache.shenyu.register.client.polaris.enums.OpenApiURIEnum;
import org.apache.shenyu.register.client.polaris.model.ConfigFileRelease;
import org.apache.shenyu.register.client.polaris.model.ConfigFileTemp;
import org.apache.shenyu.register.client.polaris.model.ConfigFilesResponse;
import org.apache.shenyu.register.client.polaris.model.ResponseResult;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.tencent.polaris.api.exception.ErrorCode;
import com.tencent.polaris.api.exception.PolarisException;
import org.apache.commons.lang3.StringUtils;
import org.apache.hc.core5.http.*;
import org.apache.hc.core5.http.message.BasicClassicHttpRequest;
import org.apache.hc.core5.http.message.BasicHeader;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;


public class PolarisConfigClient {

    private final static Logger LOGGER = LoggerFactory.getLogger(PolarisConfigClient.class);

    private String addresses;

    private String token;

    private HttpOperator httpOperator = new HttpOperator();

    private final static String HTTP_HEAD_NAME_TOKEN = "X-Polaris-Token";

    public PolarisConfigClient(Properties properties){
        String addresses = properties.getProperty(ParameterKeyConstant.OPEN_API_ADDRESSES_KEY);
        if (StringUtils.isBlank(addresses)){
            LOGGER.error("The address of the Polaris configuration center openapi is missing");
            throw new PolarisException(ErrorCode.INVALID_CONFIG,"The address of the Polaris configuration center openapi is missing");
        }
        this.addresses = addresses.endsWith("/") ? addresses.substring(0, addresses.length() - 2) : addresses;
        this.token = properties.getProperty(ParameterKeyConstant.OPEN_API_TOKEN_KEY);
    }

    public ConfigFilesResponse createConfigFile(ConfigFileTemp file) throws IOException {
        List<Header> headers = new ArrayList<>();
        headers.add(new BasicHeader(HTTP_HEAD_NAME_TOKEN, token));
        headers.add(new BasicHeader("Content-Type", "application/json"));

        BasicClassicHttpRequest request = httpOperator.buildReqOnBody(addresses + OpenApiURIEnum.URI_CREATE_CONFIG.getUri(), OpenApiURIEnum.URI_CREATE_CONFIG.getMethod(), headers, marshalJsonText(file));
        ResponseResult result = httpOperator.send(request);
        return GsonUtils.getInstance().fromJson(result.getData(), ConfigFilesResponse.class);
    }

    public ConfigFilesResponse updateConfigFile(ConfigFileTemp file) throws IOException {
        List<Header> headers = new ArrayList<>();
        headers.add(new BasicHeader(HTTP_HEAD_NAME_TOKEN, token));
        headers.add(new BasicHeader("Content-Type", "application/json"));

        BasicClassicHttpRequest request = httpOperator.buildReqOnBody(addresses + OpenApiURIEnum.URI_UPDATE_CONFIG.getUri(), OpenApiURIEnum.URI_UPDATE_CONFIG.getMethod(), headers, marshalJsonText(file));
        ResponseResult result = httpOperator.send(request);
        return GsonUtils.getInstance().fromJson(result.getData(), ConfigFilesResponse.class);
    }

    public ConfigFilesResponse releaseConfigFile(ConfigFileRelease file) throws IOException {
        List<Header> headers = new ArrayList<>();
        headers.add(new BasicHeader(HTTP_HEAD_NAME_TOKEN, token));
        headers.add(new BasicHeader("Content-Type", "application/json"));

        BasicClassicHttpRequest request = httpOperator.buildReqOnBody(addresses + OpenApiURIEnum.URI_RELEASE_CONFIG.getUri(), OpenApiURIEnum.URI_RELEASE_CONFIG.getMethod(), headers, marshalJsonText(file));
        ResponseResult result = httpOperator.send(request);
        return GsonUtils.getInstance().fromJson(result.getData(), ConfigFilesResponse.class);
    }

    public ConfigFilesResponse getConfigFile(ConfigFileRelease file) throws IOException {
        List<NameValuePair> nameValuePairs = new ArrayList<>();
        BasicNameValuePair name = new BasicNameValuePair("name",file.getName());
        BasicNameValuePair namespace = new BasicNameValuePair("namespace",file.getNamespace());
        BasicNameValuePair group = new BasicNameValuePair("group",file.getGroup());
        nameValuePairs.add(name);
        nameValuePairs.add(namespace);
        nameValuePairs.add(group);

        List<Header> headers = new ArrayList<>();
        BasicHeader basicHeader = new BasicHeader(HTTP_HEAD_NAME_TOKEN, token);
        headers.add(basicHeader);
        try {
            BasicClassicHttpRequest request = httpOperator.buildReqOnUrlParam(addresses + OpenApiURIEnum.URI_GET_CONFIG.getUri(), OpenApiURIEnum.URI_GET_CONFIG.getMethod(), headers, nameValuePairs);
            ResponseResult result = httpOperator.send(request);
            return GsonUtils.getInstance().fromJson(result.getData(), ConfigFilesResponse.class);
        } catch (URISyntaxException e) {
            throw new RuntimeException("The Polaris openapi interface is not valid");
        }
    }


    public String marshalJsonText(Object value) {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_EMPTY);
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            LOGGER.error("[Core] fail to serialize object {}", value, e);
        }
        return "";
    }
}

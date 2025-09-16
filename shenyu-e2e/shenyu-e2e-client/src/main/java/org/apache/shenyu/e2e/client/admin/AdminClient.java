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

package org.apache.shenyu.e2e.client.admin;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.annotation.ShenYuAdminClient;
import org.apache.shenyu.e2e.client.BaseClient;
import org.apache.shenyu.e2e.common.IdManagers.Rules;
import org.apache.shenyu.e2e.common.IdManagers.Selectors;
import org.apache.shenyu.e2e.common.NameUtils;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.ShenYuResult;
import org.apache.shenyu.e2e.model.data.BindingData;
import org.apache.shenyu.e2e.model.data.NamespaceSyncData;
import org.apache.shenyu.e2e.model.data.QueryCondition;
import org.apache.shenyu.e2e.model.data.ResourceData;
import org.apache.shenyu.e2e.model.data.RuleData;
import org.apache.shenyu.e2e.model.data.RuleQueryCondition;
import org.apache.shenyu.e2e.model.data.SearchCondition;
import org.apache.shenyu.e2e.model.data.SelectorData;
import org.apache.shenyu.e2e.model.data.SelectorQueryCondition;
import org.apache.shenyu.e2e.model.response.FakeResourceDTO;
import org.apache.shenyu.e2e.model.response.LoginInfo;
import org.apache.shenyu.e2e.model.response.MetaDataDTO;
import org.apache.shenyu.e2e.model.response.PaginatedResources;
import org.apache.shenyu.e2e.model.response.PluginDTO;
import org.apache.shenyu.e2e.model.response.NamespacePluginDTO;
import org.apache.shenyu.e2e.model.response.ResourceDTO;
import org.apache.shenyu.e2e.model.response.RuleDTO;
import org.apache.shenyu.e2e.model.response.SearchedResources;
import org.apache.shenyu.e2e.model.response.SelectorDTO;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.apache.shenyu.e2e.constant.Constants.SYS_DEFAULT_NAMESPACE_NAMESPACE_ID;
import static org.apache.shenyu.e2e.model.data.SearchCondition.QUERY_ALL;

/**
 * A client to connect to ShenYu Admin.
 */
@ShenYuAdminClient
public class AdminClient extends BaseClient {

    private static final Logger log = LoggerFactory.getLogger(AdminClient.class);

    private static final TypeReference<PaginatedResources<PluginDTO>> PAGINATED_PLUGINS_TYPE_REFERENCE = new TypeReference<>() {
    };

    private static final TypeReference<SearchedResources<SelectorDTO>> SEARCHED_SELECTORS_TYPE_REFERENCE = new TypeReference<>() {
    };

    private static final TypeReference<SearchedResources<RuleDTO>> SEARCHED_RULES_TYPE_REFERENCE = new TypeReference<>() {
    };

    private static final TypeReference<SearchedResources<FakeResourceDTO>> FAKE_VALUE_TYPE = new TypeReference<>() {
    };

    private static final TypeReference<List<MetaDataDTO>> SEARCHED_METADATAS_TYPE_REFERENCE = new TypeReference<>() {
    };

    private final MultiValueMap<String, String> basicAuth = new HttpHeaders();

    private final RestTemplate template = new RestTemplate();

    private final ObjectMapper mapper = new ObjectMapper();

    private final String scenarioId;

    private final String baseURL;

    private String serviceName;

    private final ImmutableMap<String, String> loginInfo;

    public AdminClient(final String scenarioId, final String serviceName, final String baseURL, final Properties properties) {
        super(serviceName);
        Preconditions.checkArgument(properties.containsKey("username"), "Property username does not exist");
        Preconditions.checkArgument(properties.containsKey("password"), "Property password does not exist");
        this.baseURL = baseURL;
        this.scenarioId = scenarioId;
        this.serviceName = serviceName;
        this.loginInfo = ImmutableMap.<String, String>builder()
                .put("username", properties.getProperty("username"))
                .put("password", properties.getProperty("password"))
                .build();
    }

    /**
     * Login to ShenYu Admin and cache the token.
     */
    public void login() {
        final String url = baseURL + "/platform/login?userName={username}&password={password}";
        ResponseEntity<ShenYuResult> response = template.getForEntity(
                url,
                ShenYuResult.class,
                loginInfo
        );
        ShenYuResult rst = assertAndGet(response, "login dashboard user success");

        String token = Assertions.assertDoesNotThrow(() -> rst.toObject(LoginInfo.class).getToken(), "checking to cast common");
        Assertions.assertNotNull(token, "checking token not null");
        Assertions.assertNotEquals("", token, "checking token not empty");
        basicAuth.set("X-Access-Token", token);

        Plugin.check(listPlugins());
    }

    /**
     * List all plugins.
     *
     * @return a list of {@link PluginDTO}s
     */
    public List<PluginDTO> listPlugins() {
        List<PluginDTO> result = Lists.newArrayList();

        int cur = 1;
        int total;
        do {
            ResponseEntity<ShenYuResult> response = template.exchange(
                    baseURL + "/plugin-template?currentPage={cur}&pageSize={page}",
                    HttpMethod.GET,
                    new HttpEntity<>(basicAuth),
                    ShenYuResult.class,
                    cur,
                    30
            );
            ShenYuResult rst = assertAndGet(response, "query success");

            PaginatedResources<PluginDTO> pagination = Assertions.assertDoesNotThrow(
                () -> mapper.readValue(rst.getData().traverse(), PAGINATED_PLUGINS_TYPE_REFERENCE),
                "checking cast to PaginatedResources<T>"
            );
            result.addAll(pagination.getDataList());

            total = pagination.getPage().getTotalPage();
        } while (++cur < total);
        return result;
    }

    /**
     * List all existence selectors.
     *
     * @return a list of {@link SelectorDTO}s
     */
    public List<SelectorDTO> listAllSelectors() {
        SelectorQueryCondition condition = SelectorQueryCondition.builder()
                .switchStatus(true)
                .build();
        return list("/selector/list/search", condition, SEARCHED_SELECTORS_TYPE_REFERENCE, v -> v);
    }

    /**
     * List all existence rules.
     *
     * @return a list of {@link RuleDTO}s
     */
    public List<RuleDTO> listAllRules() {
        RuleQueryCondition condition = RuleQueryCondition.builder()
                .switchStatus(true)
                .build();
        return list("/rule/list/search", condition, SEARCHED_RULES_TYPE_REFERENCE, v -> v);
    }

    /**
     * all meta data list.
     *
     * @return List
     */
    public List<MetaDataDTO> listAllMetaData() {
        return getMetaDataList("/meta-data/findAll", SEARCHED_METADATAS_TYPE_REFERENCE, v -> v);
    }

    private <T extends ResourceDTO, OUT> List<OUT> list(final String uri, final QueryCondition condition, final TypeReference<SearchedResources<T>> valueType, final Mapper<T, OUT> mapper) {
        List<OUT> result = Lists.newArrayList();

        int curPage = 1;
        int total;

        do {
            SearchedResources<T> resources = search(uri, curPage, 20, condition, valueType);
            resources.getList().stream()
                    .map(mapper)
                    .forEach(result::add);
            total = resources.getPages();
        } while (++curPage <= total);

        return result;
    }

    private <T extends ResourceDTO, OUT> List<OUT> getMetaDataList(final String uri, final TypeReference<List<T>> valueType, final Mapper<T, OUT> mapper) {
        List<OUT> result = Lists.newArrayList();
        List<T> resources = getSearch(uri, valueType);
        resources.stream()
                .map(mapper)
                .forEach(result::add);
        return result;
    }

    /**
     * Fetch the selectors by the given conditions.
     *
     * @param keyword expected selectors included the word. return all if absent.
     * @param plugins expected selectors under specified plugins. return all if absent.
     * @return paginated info with  list of {@link SelectorDTO}s
     */
    public SearchedResources<SelectorDTO> searchSelectors(final String keyword, final String... plugins) {
        SelectorQueryCondition condition = SelectorQueryCondition.builder()
                .keyword(keyword)
                .plugins(plugins)
                .switchStatus(true)
                .namespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID)
                .build();
        return search("/selector/list/search", condition, SEARCHED_SELECTORS_TYPE_REFERENCE);
    }

    /**
     * Fetch the selectors by the given conditions.
     *
     * @param keyword  expected selectors included the word. return all if absent.
     * @param page     page.
     * @param pageSize size.
     * @param plugins  expected selectors under specified plugins. return all if absent.
     * @return paginated info with  list of {@link SelectorDTO}s
     */
    public SearchedResources<SelectorDTO> searchSelectors(final String keyword, final int page, final int pageSize, final String... plugins) {
        SelectorQueryCondition condition = SelectorQueryCondition.builder()
                .keyword(keyword)
                .plugins(plugins)
                .switchStatus(true)
                .build();
        return search("/selector/list/search", page, pageSize, condition, SEARCHED_SELECTORS_TYPE_REFERENCE);
    }

    /**
     * Fetch the rules by the given conditions.
     *
     * @param keyword   expected selectors included the word. return all if absent.
     * @param selectors expected selectors under specified plugins. return all if absent.
     * @return paginated info with list of {@link RuleDTO}s
     */
    public SearchedResources<RuleDTO> searchRules(final String keyword, final String... selectors) {
        RuleQueryCondition condition = RuleQueryCondition.builder()
                .keyword(keyword)
                .selectors(selectors)
                .namespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID)
                .switchStatus(true)
                .build();
        return search("/rule/list/search", condition, SEARCHED_RULES_TYPE_REFERENCE);
    }

    private <T extends ResourceDTO> SearchedResources<T> search(final String uri, final QueryCondition condition, final TypeReference<SearchedResources<T>> valueType) {
        return search(uri, 1, 10, condition, valueType);
    }

    private <T extends ResourceDTO> SearchedResources<T> search(final String uri, final int pageNum, final int pageSize,
                                                                final QueryCondition condition, final TypeReference<SearchedResources<T>> valueType) {
        SearchCondition searchCondition = SearchCondition.builder()
                .pageNum(pageNum)
                .pageSize(pageSize)
                .condition(condition)
                .build();

        HttpEntity<SearchCondition> entity = new HttpEntity<>(searchCondition, basicAuth);
        ResponseEntity<ShenYuResult> response = template.postForEntity(baseURL + uri, entity, ShenYuResult.class);
        ShenYuResult rst = assertAndGet(response, "query success");

        return Assertions.assertDoesNotThrow(() -> mapper.readValue(
            rst.getData().traverse(), valueType),
            "checking cast to SearchedResources<T>"
        );
    }

    private <T extends ResourceDTO> List<T> getSearch(final String uri, final TypeReference<List<T>> valueType) {
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri, HttpMethod.GET, new HttpEntity<>(basicAuth), ShenYuResult.class);
        ShenYuResult rst = assertAndGet(response, "query success");
        return Assertions.assertDoesNotThrow(
            () -> mapper.readValue(rst.getData().traverse(), valueType),
            "checking cast to SearchedResources<T>"
        );
    }

    /**
     * create selectorDTO.
     *
     * @param selector selector
     * @return SelectorDTO
     */
    public SelectorDTO create(final SelectorData selector) {
        selector.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        SelectorDTO dto = create("/selector", selector);
        Selectors.INSTANCE.put(selector.getName(), dto.getId());
        return dto;
    }

    /**
     * Create Rule.
     *
     * @param rule rule
     * @return RuleDTO
     */
    public RuleDTO create(final RuleData rule) {
        rule.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        rule.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        RuleDTO dto = create("/rule", rule);
        Rules.INSTANCE.put(rule.getName(), dto.getId());
        return dto;
    }

    @SuppressWarnings("unchecked")
    private <T extends ResourceData, R extends ResourceDTO> R create(final String uri, final T data) {
        log.info("trying to create resource({}) name: {}", data.getClass().getSimpleName(), data.getName());

        data.setName(NameUtils.wrap(data.getName(), scenarioId));

        HttpEntity<T> entity = new HttpEntity<>(data, basicAuth);
        ResponseEntity<ShenYuResult> response = template.postForEntity(baseURL + uri, entity, ShenYuResult.class);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "status code");

        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        Assertions.assertEquals(200, rst.getCode(), "checking shenyu result code");
        Assertions.assertEquals("create success", rst.getMessage(), "checking shenyu result message");

        SearchedResources<?> searchedResources = null;
        if (data instanceof SelectorData) {
            searchedResources = searchSelectors(data.getName());
        } else if (data instanceof RuleData) {
            searchedResources = searchRules(data.getName());
        }
        Assertions.assertNotNull(searchedResources, "checking searchedResources object is non-null");
        Assertions.assertEquals(1, searchedResources.getTotal(), "checking the total hits of searching");

        ResourceDTO created = searchedResources.getList().get(0);
        Assertions.assertNotNull(created, "checking created object is non-null");
        log.info("create resource({}) successful. name: {}, id: {}", data.getClass().getSimpleName(), data.getName(), created.getId());

        return (R) created;
    }

    /**
     * bindingData.
     *
     * @param bindingData bindingData
     */
    public void bindingData(final BindingData bindingData) {
        bindingData.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        HttpEntity<BindingData> entity = new HttpEntity<>(bindingData, basicAuth);
        ResponseEntity<ShenYuResult> response = template.postForEntity(baseURL + "/proxy-selector/binding", entity, ShenYuResult.class);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "status code");
    }

    /**
     * Delete selectors in batch.
     *
     * @param ids ID of selectors that needs to delete.
     */
    public void deleteSelectors(final List<String> ids) {
        delete("/selector/batch", ids);
    }

    /**
     * Delete selectors in batch.
     *
     * @param ids ID of selectors that needs to delete.
     */
    public void deleteSelectors(final String... ids) {
        delete("/selector/batch", Lists.newArrayList(ids));
    }

    /**
     * Delete rules in batch.
     *
     * @param ids ID of rules that needs to delete.
     */
    public void deleteRules(final String... ids) {
        delete("/rule/batch", Lists.newArrayList(ids));
    }

    /**
     * Delete all selectors.
     */
    public void deleteAllSelectors() {
        deleteAll("/selector/batch");
    }

    /**
     * Delete all rules under given id of selector.
     *
     * @param selectorId ID of selector
     */
    public void deleteAllRules(final String selectorId) {
        List<String> ids = searchRules(null, selectorId).getList().stream().map(RuleDTO::getId).collect(Collectors.toList());
        deleteRules(ids.toArray(new String[]{}));
    }

    private void deleteAll(final String uri) {
        Preconditions.checkArgument(uri.endsWith("/batch"), "uri[{}] must be end with '/batch'", uri);

        String listAllResourcesUrl = uri.replace("/batch", "") + "/list/search";
        List<String> ids = list(listAllResourcesUrl, QUERY_ALL, FAKE_VALUE_TYPE, FakeResourceDTO::getId);

        delete(uri, ids);
        List<FakeResourceDTO> result = list(listAllResourcesUrl, QUERY_ALL, FAKE_VALUE_TYPE, v -> v);
        Assertions.assertEquals(0, result.size(), "resource list is empty after deleted");
    }

    private void delete(final String uri, final List<String> ids) {
        if (ids.isEmpty()) {
            log.info("delete resources, effected size: 0, cause by: there is not resources in ShenYuAdmin");
            return;
        }
        Map<String, Object> body = new HashMap<>();
        body.put("ids", ids);
        body.put("namespaceId", SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(body, basicAuth);
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri, HttpMethod.DELETE, entity, ShenYuResult.class);
        ShenYuResult rst = assertAndGet(response, "delete success");
        Integer deleted = Assertions.assertDoesNotThrow(() -> rst.toObject(Integer.class), "checking to cast object");
        Assertions.assertEquals(ids.size(), deleted, "checking deleted records");

        log.info("delete resources, effected size: {}, effected rows: {}", ids.size(), ids);
    }

    /**
     * Fetch Selector by given id.
     *
     * @param id of selector that needs to fetch
     * @return {@link SelectorDTO}
     */
    public SelectorDTO getSelector(final String id) {
        return getResource("/selector", id, SelectorDTO.class);
    }

    private <T extends ResourceDTO> T getResource(final String uri, final String id, final Class<T> valueType) {
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri + "/{id}", HttpMethod.GET, new HttpEntity<>(basicAuth), ShenYuResult.class, id);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "checking http status");

        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");

        if (rst.getCode() == 500) {
            Assertions.assertTrue(rst.getMessage().contains("selector is not existed"), "checking shenyu result message");
            return null;
        }

        Assertions.assertEquals("detail success", rst.getMessage(), "checking shenyu result message");
        Assertions.assertEquals(200, rst.getCode(), "checking shenyu result code");
        return Assertions.assertDoesNotThrow(() -> rst.toObject(valueType), "checking cast data to " + valueType.getSimpleName());
    }

    private ShenYuResult assertAndGet(final ResponseEntity<ShenYuResult> response, final String message) {
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "checking http status");

        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        Assertions.assertEquals(200, rst.getCode(), "checking shenyu result code");
        Assertions.assertEquals(message, rst.getMessage(), "checking shenyu result message");

        return rst;
    }

    /**
     * change plugin status.
     *
     * @param id       id
     * @param requestBody requestBody
     */
    public void changePluginStatus(final String id, final Map<String, String> requestBody) {
        putResource("/namespace-plugin", id, NamespacePluginDTO.class, requestBody);
    }

    private <T extends ResourceDTO> T putResource(final String uri, final String id, final Class<T> valueType, final Map<String, String> requestBody) {
        HttpEntity<Map<String, String>> requestEntity = new HttpEntity<>(requestBody, basicAuth);
        
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri + "/" + id, HttpMethod.PUT, requestEntity, ShenYuResult.class);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "checking http status");
        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        return Assertions.assertDoesNotThrow(() -> rst.toObject(valueType), "checking cast data to " + valueType.getSimpleName());
    }

    /**
     * change plugin status.
     *
     * @param id           id
     * @param selectorData selectorData
     */
    public void changeSelector(final String id, final SelectorData selectorData) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            putResourceByJson("/selector", id, mapper.writeValueAsString(selectorData));
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    private void putResourceByJson(final String uri, final String id, final String json) {
        basicAuth.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
        HttpEntity<String> requestEntity = new HttpEntity<>(json, basicAuth);
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri + "/" + id, HttpMethod.PUT, requestEntity, ShenYuResult.class);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "checking http status");
        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        basicAuth.remove("Content-Type");
    }

    /**
     * sync all plugin.
     */
    public void syncPluginAll() {
        NamespaceSyncData namespaceSyncData = new NamespaceSyncData();
        namespaceSyncData.setNamespaceId(SYS_DEFAULT_NAMESPACE_NAMESPACE_ID);
        basicAuth.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
        HttpEntity<NamespaceSyncData> entity = new HttpEntity<>(namespaceSyncData, basicAuth);
        template.postForEntity(baseURL + "/namespace-plugin/syncPluginAll", entity, ShenYuResult.class);
        log.warn("admin syncPluginAll");
    }

    @FunctionalInterface
    interface Mapper<I, O> extends Function<I, O> {

    }

}

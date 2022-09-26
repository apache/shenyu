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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.e2e.annotation.ShenYuAdminClient;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.ShenYuResult;
import org.apache.shenyu.e2e.client.admin.model.data.ResourceData;
import org.apache.shenyu.e2e.client.admin.model.data.RuleData;
import org.apache.shenyu.e2e.client.admin.model.data.SearchCondition;
import org.apache.shenyu.e2e.client.admin.model.data.SearchCondition.QueryCondition;
import org.apache.shenyu.e2e.client.admin.model.data.SearchCondition.RuleQueryCondition;
import org.apache.shenyu.e2e.client.admin.model.data.SearchCondition.SelectorQueryCondition;
import org.apache.shenyu.e2e.client.admin.model.data.SelectorData;
import org.apache.shenyu.e2e.client.admin.model.response.FakeResourceDTO;
import org.apache.shenyu.e2e.client.admin.model.response.LoginInfo;
import org.apache.shenyu.e2e.client.admin.model.response.PaginatedResources;
import org.apache.shenyu.e2e.client.admin.model.response.PluginDTO;
import org.apache.shenyu.e2e.client.admin.model.response.ResourceDTO;
import org.apache.shenyu.e2e.client.admin.model.response.RuleDTO;
import org.apache.shenyu.e2e.client.admin.model.response.SearchedResources;
import org.apache.shenyu.e2e.client.admin.model.response.SelectorDTO;
import org.apache.shenyu.e2e.common.IdManagers.Rules;
import org.apache.shenyu.e2e.common.IdManagers.Selectors;
import org.apache.shenyu.e2e.common.NameUtils;
import org.junit.jupiter.api.Assertions;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

@Slf4j
@ShenYuAdminClient
public class AdminClient {
    private final MultiValueMap<String, String> basicAuth = new HttpHeaders();
    private final RestTemplate template = new RestTemplateBuilder().build();
    private final ObjectMapper mapper = new ObjectMapper();
    
    private final String scenarioId;
    
    private final String baseURL;
    private final Properties properties;
    private final ImmutableMap<String, String> loginInfo;
    
    
    private static final TypeReference<PaginatedResources<SelectorDTO>> PAGINATED_SELECTORS_TYPE_REFERENCE = new TypeReference<>() {
    };
    private static final TypeReference<PaginatedResources<RuleDTO>> PAGINATED_RULES_TYPE_REFERENCE = new TypeReference<>() {
    };
    private static final TypeReference<PaginatedResources<PluginDTO>> PAGINATED_PLUGINS_TYPE_REFERENCE = new TypeReference<>() {
    };
    
    private static final TypeReference<SearchedResources<SelectorDTO>> SEARCHED_SELECTORS_TYPE_REFERENCE = new TypeReference<>() {
    };
    private static final TypeReference<SearchedResources<RuleDTO>> SEARCHED_RULES_TYPE_REFERENCE = new TypeReference<>() {
    };
    private static final TypeReference<SearchedResources<PluginDTO>> SEARCHED_PLUGINS_TYPE_REFERENCE = new TypeReference<>() {
    };
    private static final TypeReference<PaginatedResources<FakeResourceDTO>> FAKE_VALUE_TYPE = new TypeReference<>() {
    };
    
    @Deprecated
    public AdminClient(String baseURL, String username, String password) {
        this.baseURL = baseURL;
        this.scenarioId = "";
        this.loginInfo = ImmutableMap.<String, String>builder()
                .put("username", username)
                .put("password", password)
                .build();
        this.properties = null;
    }
    
    public AdminClient(String scenarioId, String baseURL, Properties properties) {
        Preconditions.checkArgument(properties.containsKey("username"), "Property username does not exist");
        Preconditions.checkArgument(properties.containsKey("password"), "Property password does not exist");
        
        this.baseURL = baseURL;
        this.scenarioId = scenarioId;
        this.properties = properties;
        this.loginInfo = ImmutableMap.<String, String>builder()
                .put("username", properties.getProperty("username"))
                .put("password", properties.getProperty("password"))
                .build();
    }
    
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
    
    public List<PluginDTO> listPlugins() {
        return list("/plugin", PAGINATED_PLUGINS_TYPE_REFERENCE);
    }
    
    public List<SelectorDTO> listAllSelectors() {
        return list("/selector", PAGINATED_SELECTORS_TYPE_REFERENCE);
    }
    
    public List<RuleDTO> listAllRules() {
        return list("/rule", PAGINATED_RULES_TYPE_REFERENCE);
    }
    
    private <T extends ResourceDTO, OUT> List<OUT> list(String uri, TypeReference<PaginatedResources<T>> valueType, Mapper<T, OUT> map) {
        String url = baseURL + uri + "?currentPage={cur}&pageSize={size}";
        List<OUT> result = Lists.newArrayList();
        
        int cur = 1;
        while (true) {
            ResponseEntity<ShenYuResult> response = template.exchange(
                    url,
                    HttpMethod.GET,
                    new HttpEntity<>(basicAuth),
                    ShenYuResult.class,
                    cur,
                    10
            );
            ShenYuResult rst = assertAndGet(response, "query success");
            
            PaginatedResources<T> pagination = Assertions.assertDoesNotThrow(
                    () -> mapper.readValue(rst.getData().traverse(), valueType),
                    "checking cast to PaginatedResources<T>"
            );
            pagination.getDataList().forEach(e -> result.add(map.map(e)));
            
            if (cur >= pagination.getPage().getTotalPage()) {
                break;
            }
            cur++;
        }
        return result;
    }
    
    private <T extends ResourceDTO> List<T> list(String uri, TypeReference<PaginatedResources<T>> valueType) {
        return list(uri, valueType, value -> value);
    }
    
    public List<SelectorDTO> searchSelector(String keyword, String... plugins) {
        SelectorQueryCondition condition = SelectorQueryCondition.builder()
                .keyword(keyword)
                .plugins(plugins)
                .switchStatus(true)
                .build();
        return search("/selector/list/search", condition, SEARCHED_SELECTORS_TYPE_REFERENCE).getList();
    }
    
    public List<RuleDTO> searchRule(String keyword, String... selectors) {
        RuleQueryCondition condition = RuleQueryCondition.builder()
                .keyword(keyword)
                .selectors(selectors)
                .switchStatus(true)
                .build();
        return search("/rule/list/search", condition, SEARCHED_RULES_TYPE_REFERENCE).getList();
    }
    
    private <T extends ResourceDTO> SearchedResources<T> search(String uri, QueryCondition condition, TypeReference<SearchedResources<T>> valueType) {
        return search(uri, 1, 10, condition, valueType);
    }
    
    private <T extends ResourceDTO> SearchedResources<T> search(String uri, int pageNum, int pageSize, QueryCondition condition, TypeReference<SearchedResources<T>> valueType) {
        SearchCondition searchCondition = SearchCondition.builder()
                .pageNum(pageNum)
                .pageSize(pageSize)
                .condition(condition)
                .build();
        
        HttpEntity<SearchCondition> entity = new HttpEntity<>(searchCondition, basicAuth);
        ResponseEntity<ShenYuResult> response = template.postForEntity(baseURL + uri, entity, ShenYuResult.class);
        ShenYuResult rst = assertAndGet(response, "query success");
        
        return Assertions.assertDoesNotThrow(
                () -> mapper.readValue(rst.getData().traverse(), valueType),
                "checking cast to SearchedResources<T>"
        );
    }
    
    public SelectorDTO create(SelectorData selector) {
        SelectorDTO dto = create("/selector", selector);
        Selectors.INSTANCE.put(selector.getName(), dto.getId());
        return dto;
    }
    
    public RuleDTO create(RuleData rule) {
        RuleDTO dto = create("/rule", rule);
        Rules.INSTANCE.put(rule.getName(), dto.getId());
        return dto;
    }
    
    private <T extends ResourceData, R extends ResourceDTO> R create(String uri, T data) {
        log.info("trying to create resource({}) name: {}", data.getClass().getSimpleName(), data.getName());
        
        data.setName(NameUtils.wrap(data.getName(), scenarioId));
        
        HttpEntity<T> entity = new HttpEntity<>(data, basicAuth);
        ResponseEntity<ShenYuResult> response = template.postForEntity(baseURL + uri, entity, ShenYuResult.class);
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "status code");
        
        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        Assertions.assertEquals(200, rst.getCode(), "checking shenyu result code");
        Assertions.assertEquals("create success", rst.getMessage(), "checking shenyu result message");
        
        ResourceDTO created = null;
        if (data instanceof SelectorData) {
            created = searchSelector(data.getName()).get(0);
        } else if (data instanceof RuleData) {
            created = searchRule(data.getName()).get(0);
        }
        Assertions.assertNotNull(created, "checking created object is non-null");
        log.info("create resource({}) successful. name: {}, id: {}", data.getClass().getSimpleName(), data.getName(), created.getId());
        
        return (R) created;
    }
    
    public void deleteSelectors(List<String> ids) {
        delete("/selector/batch", ids);
    }
    
    public void deleteSelectors(String... ids) {
        delete("/selector/batch", Lists.newArrayList(ids));
    }
    
    public void deleteRules(String... ids) {
        delete("/rule/batch", Lists.newArrayList(ids));
    }
    
    public void deleteAllSelectors() {
        deleteAll("/selector/batch");
    }
    
    public void deleteAllRules(String selectorId) {
        List<String> ids = searchRule(null, selectorId).stream().map(RuleDTO::getId).collect(Collectors.toList());
        deleteRules(ids.toArray(new String[]{}));
    }
    
    private void deleteAll(String uri) {
        Preconditions.checkArgument(uri.endsWith("/batch"), "uri[{}] must be end with '/batch'", uri);
        
        String listAllResourcesUrl = uri.replace("/batch", "");
        List<String> ids = list(listAllResourcesUrl, FAKE_VALUE_TYPE, FakeResourceDTO::getId);
        
        delete(uri, ids);
        
        List<FakeResourceDTO> result = list(listAllResourcesUrl, FAKE_VALUE_TYPE);
        Assertions.assertTrue(result.isEmpty(), "checking whether resource list is empty after deleted");
    }
    
    private void delete(String uri, List<String> ids) {
        if (ids.isEmpty()) {
            log.info("delete resources, effected size: 0, cause by: there is not resources in ShenYuAdmin");
            return;
        }
        
        HttpEntity<List<String>> entity = new HttpEntity<>(ids, basicAuth);
        ResponseEntity<ShenYuResult> response = template.exchange(baseURL + uri, HttpMethod.DELETE, entity, ShenYuResult.class);
        ShenYuResult rst = assertAndGet(response, "delete success");
        Integer deleted = Assertions.assertDoesNotThrow(() -> rst.toObject(Integer.class), "checking to cast object");
        Assertions.assertEquals(ids.size(), deleted, "checking deleted records");
        
        log.info("delete resources, effected size: {}, effected rows: {}", ids.size(), ids);
    }
    
    public <T extends ResourceDTO> T getResource(String uri, QueryCondition condition, TypeReference<SearchedResources<T>> valueType) {
        SearchedResources<T> searchedResources = (SearchedResources<T>) search(uri, condition, valueType);
        Assertions.assertEquals(1, searchedResources.getTotal(), "checking the total hits of searching");
        return searchedResources.getList().get(0);
    }
    
    private ShenYuResult assertAndGet(ResponseEntity<ShenYuResult> response, String message) {
        Assertions.assertEquals(HttpStatus.OK, response.getStatusCode(), "checking http status");
        
        ShenYuResult rst = response.getBody();
        Assertions.assertNotNull(rst, "checking http response body");
        Assertions.assertEquals(200, rst.getCode(), "checking shenyu result code");
        Assertions.assertEquals(message, rst.getMessage(), "checking shenyu result message");
        
        return rst;
    }
    
    @FunctionalInterface
    interface Mapper<IN, OUT> {
        OUT map(IN value);
    }
}

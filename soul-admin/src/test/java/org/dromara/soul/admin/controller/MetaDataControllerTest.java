package org.dromara.soul.admin.controller;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.BatchCommonDTO;
import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.MetaDataQuery;
import org.dromara.soul.admin.service.MetaDataService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.MetaDataVO;
import org.dromara.soul.common.utils.DateUtils;
import org.dromara.soul.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.*;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for MetaDataController.
 *
 * @author fcwalker
 */
@RunWith(MockitoJUnitRunner.class)
public class MetaDataControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private MetaDataController metaDataController;

    @Mock
    private MetaDataService metaDataService;

    private final MetaDataVO metaDataVO = new MetaDataVO("appName", "appPath", "desc", "rpcType", "serviceName", "methodName", "types", "rpcExt",
            "1", DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()),
            true);

    private final PageParameter pageParameter = new PageParameter();

    private final CommonPager commonPager = new CommonPager(pageParameter, Arrays.asList(metaDataVO));

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(metaDataController).build();
    }

    @Test
    public void testQueryList() throws Exception {
        given(this.metaDataService.listByPage(new MetaDataQuery("appName", pageParameter))).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/queryList").param("appName", "appName").param("currentPage", pageParameter.getCurrentPage() + "").param("pageSize", pageParameter.getPageSize() + ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testFindAll() throws Exception {
        given(this.metaDataService.findAll()).willReturn(Arrays.asList(metaDataVO));
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/findAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testFindAllGroup() throws Exception {
        final Map<String, List<MetaDataVO>> result = new HashMap<>();
        String groupName = "groupName-1";
        result.put(groupName, Arrays.asList(metaDataVO));
        given(this.metaDataService.findAllGroup()).willReturn(result);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/findAllGroup"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data." + groupName + "[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testEditor() throws Exception {
        given(this.metaDataService.findById("1")).willReturn(metaDataVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/{id}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testCreateOrUpdate() throws Exception {
        final MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("0001");
        metaDataDTO.setAppName("aname-01");
        metaDataDTO.setContextPath("path");
        metaDataDTO.setEnabled(false);
        given(this.metaDataService.createOrUpdate(metaDataDTO)).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/createOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testBatchDeleted() throws Exception {
        final List<String> ids = new ArrayList<>(2);
        ids.add("1");
        ids.add("2");
        given(this.metaDataService.delete(ids)).willReturn(2);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/batchDeleted")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(ids)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(2)))
                .andReturn();
    }

    @Test
    public void testBatchEnabled() throws Exception {
        final BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setIds(Arrays.asList("1","2"));
        batchCommonDTO.setEnabled(true);
        given(this.metaDataService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.ENABLE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testSyncData() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/syncData"))
                .andExpect(status().isOk())
                .andReturn();
    }
}

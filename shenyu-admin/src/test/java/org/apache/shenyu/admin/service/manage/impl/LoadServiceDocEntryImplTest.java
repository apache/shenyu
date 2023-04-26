package org.apache.shenyu.admin.service.manage.impl;

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.manager.ServiceDocManager;
import org.apache.shenyu.admin.service.manager.impl.DocManagerImpl;
import org.apache.shenyu.admin.service.manager.impl.LoadServiceDocEntryImpl;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;

import static org.mockito.Mockito.mock;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LoadServiceDocEntryImplTest {

    @InjectMocks
    private LoadServiceDocEntryImpl loadServiceDocEntry;

    @Mock
    private  SelectorService selectorService;
    @Mock
    private  SelectorHandleConverterFactor converterFactor;
    @Mock
    private  PluginMapper pluginMapper;
    @Mock
    private  ServiceDocManager serviceDocManager;
    @Mock
    private  ShenyuDictService shenyuDictService;

    @Test
    public void testLoadApiDocument(){
        ShenyuDictVO shenyuInitData = new ShenyuDictVO();
        shenyuInitData.setDictValue("true");
        when(shenyuDictService.findByDictCodeName(any(),any())).thenReturn(shenyuInitData);
        List<PluginDO> pluginDOList = new ArrayList<>();
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId("1");
        pluginDO.setName("test");
        pluginDOList.add(pluginDO);
        CommonPager<SelectorVO> commonPager = new CommonPager<>();
        List list = new ArrayList<>();
        SelectorVO selectorVO = new SelectorVO("1","1","test",1,"testMatchMode",1,"testType",1,true,true,true,true,"[{\"weight\":1}]",new ArrayList<>(),"now","now");

        list.add(selectorVO);
        commonPager.setDataList(list);
        commonPager.setPage(new PageParameter(1,1));
        SelectorHandleConverter selectorHandleConverter = mock(SelectorHandleConverter.class);
        List<CommonUpstream> upstreamList = new ArrayList<>();
        upstreamList.add(new CommonUpstream("testProtocol","testUpstreamHost","testUrl",true,1000L));

        when(selectorHandleConverter.convertUpstream(any())).thenReturn(upstreamList);
        when(converterFactor.newInstance(any())).thenReturn(selectorHandleConverter);
        when(selectorService.listByPage(any())).thenReturn(commonPager);
        when(pluginMapper.selectByNames(any())).thenReturn(pluginDOList);
        loadServiceDocEntry.loadApiDocument();

        verify(serviceDocManager).pullApiDocument((Set<UpstreamInstance>) any());
    }

    @Test
    public void testLoadDocOnSelectorChanged(){
        List<SelectorData> changedList = new ArrayList<>();
        SelectorData selectorData =new SelectorData();
        selectorData.setId("1");
        selectorData.setName("test");
        selectorData.setEnabled(true);
        selectorData.setHandle("testHandle");
        selectorData.setPluginId("1");
        selectorData.setMatchMode(1);
        changedList.add(selectorData);
        DataEventTypeEnum eventType = DataEventTypeEnum.acquireByName("CREATE");
        this.testLoadApiDocument();
        when(shenyuDictService.findByDictCodeName(any(),any())).thenReturn(null);
        loadServiceDocEntry.loadDocOnSelectorChanged(changedList,eventType);
        verify(serviceDocManager).pullApiDocument((Set<UpstreamInstance>) any());
    }
}

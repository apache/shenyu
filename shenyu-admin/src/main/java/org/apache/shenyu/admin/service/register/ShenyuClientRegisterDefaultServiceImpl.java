package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * default service register.
 *
 * @author KevinClair
 **/
@Service("default")
public class ShenyuClientRegisterDefaultServiceImpl extends AbstractShenyuClientRegisterService {

    private final ApplicationEventPublisher eventPublisher;

    private final SelectorService selectorService;

    private final SelectorMapper selectorMapper;

    public ShenyuClientRegisterDefaultServiceImpl(ApplicationEventPublisher eventPublisher, SelectorService selectorService, SelectorMapper selectorMapper) {
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.selectorMapper = selectorMapper;
    }

    @Override
    public String register(MetaDataRegisterDTO metaDataRegisterDTO) {
        return null;
    }

    @Override
    public String registerURI(String contextPath, List<String> uriList){
        SelectorDO selector = selectorService.findByName(contextPath);
        SelectorData selectorData = selectorService.buildByName(contextPath);
        String handler = GsonUtils.getInstance().toJson(buildDivideUpstreamList(uriList));
        selector.setHandle(handler);
        selectorData.setHandle(handler);
        selectorMapper.updateSelective(selector);
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                Collections.singletonList(selectorData)));
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    protected void saveOrUpdateMetaData(MetaDataDO exist, MetaDataRegisterDTO metaDataDTO) {

    }

    @Override
    protected String handlerSelector(MetaDataRegisterDTO metaDataDTO) {
        return "";
    }

    @Override
    protected void handlerRule(String selectorId, MetaDataRegisterDTO metaDataDTO, MetaDataDO exist) {

    }

    private List<DivideUpstream> buildDivideUpstreamList(final List<String> uriList) {
        return uriList.stream().map(this::buildDivideUpstream).collect(Collectors.toList());
    }

    @Override
    protected String getPluginId(String pluginName) {
        return "";
    }
}

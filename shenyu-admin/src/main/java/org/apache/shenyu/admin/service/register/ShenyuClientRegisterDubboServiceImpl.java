package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.transfer.MetaDataTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.*;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.Objects;

/**
 * dubbo service register.
 *
 * @author KevinClair
 **/
@Service("dubbo")
public class ShenyuClientRegisterDubboServiceImpl extends AbstractShenyuClientRegisterService {

    private final MetaDataMapper metaDataMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final SelectorService selectorService;

    private final PluginMapper pluginMapper;

    private final RuleMapper ruleMapper;

    private final RuleService ruleService;

    public ShenyuClientRegisterDubboServiceImpl(MetaDataMapper metaDataMapper, ApplicationEventPublisher eventPublisher, SelectorService selectorService, PluginMapper pluginMapper, RuleMapper ruleMapper, RuleService ruleService) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
        this.selectorService = selectorService;
        this.pluginMapper = pluginMapper;
        this.ruleMapper = ruleMapper;
        this.ruleService = ruleService;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public synchronized String register(final MetaDataRegisterDTO dto) {
        MetaDataDO exist = metaDataMapper.findByPath(dto.getPath());
        saveOrUpdateMetaData(exist, dto);
        String selectorId = handlerSelector(dto);
        handlerRule(selectorId, dto, exist);
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    public void saveOrUpdateMetaData(final MetaDataDO exist, final MetaDataRegisterDTO metaDataDTO) {
        DataEventTypeEnum eventType;
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapRegisterDTOToEntity(metaDataDTO);
        if (Objects.isNull(exist)) {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            metaDataDO.setId(UUIDUtils.getInstance().generateShortUuid());
            metaDataDO.setDateCreated(currentTime);
            metaDataDO.setDateUpdated(currentTime);
            metaDataMapper.insert(metaDataDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            metaDataDO.setId(exist.getId());
            metaDataMapper.update(metaDataDO);
            eventType = DataEventTypeEnum.UPDATE;
        }
        // publish MetaData's event
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, eventType,
                Collections.singletonList(MetaDataTransfer.INSTANCE.mapToData(metaDataDO))));
    }

    @Override
    public String handlerSelector(final MetaDataRegisterDTO metaDataDTO) {
        SelectorDO selectorDO = selectorService.findByName(metaDataDTO.getContextPath());
        if (Objects.nonNull(selectorDO)){
            return selectorDO.getId();
        }
        String contextPath = metaDataDTO.getContextPath();
        SelectorDTO selectorDTO = buildDefaultSelectorDTO(contextPath);
        selectorDTO.setPluginId(getPluginId(PluginEnum.DUBBO.getName()));
        selectorDTO.setSelectorConditions(buildDefaultSelectorConditionDTO(contextPath));
        return selectorService.register(registerRpcSelector(contextPath, metaDataDTO.getRpcType()));
    }

    @Override
    public String getPluginId(final String pluginName) {
        final PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        Objects.requireNonNull(pluginDO);
        return pluginDO.getId();
    }

    @Override
    public void handlerRule(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final MetaDataDO exist) {
        RuleDO existRule = ruleMapper.findByName(metaDataDTO.getPath());
        if (Objects.isNull(existRule)) {
            ruleService.register(registerRpcRule(selectorId, metaDataDTO.getPath(), PluginEnum.DUBBO.getName(), metaDataDTO.getRuleName()));
        }
    }
}

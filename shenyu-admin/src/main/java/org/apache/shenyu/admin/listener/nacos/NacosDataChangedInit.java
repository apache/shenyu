package org.apache.shenyu.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * The type Nacos data changed init.
 */
public class NacosDataChangedInit extends AbstractDataChangedInit {

    private static final Logger LOG = LoggerFactory.getLogger(NacosDataChangedInit.class);

    private final ConfigService configService;

    /**
     * Instantiates a new Nacos data changed init.
     *
     * @param configService the configService
     */
    public NacosDataChangedInit(final ConfigService configService) {
        this.configService = configService;
    }

    @Override
    protected boolean notExist() {
        return dataIdNotExist(NacosPathConstants.PLUGIN_DATA_ID)
                && dataIdNotExist(NacosPathConstants.AUTH_DATA_ID)
                && dataIdNotExist(NacosPathConstants.META_DATA_ID);
    }

    private boolean dataIdNotExist(final String pluginDataId) {
        try {
            return Objects.isNull(
                    configService.getConfig(pluginDataId,
                            NacosPathConstants.GROUP,
                            NacosPathConstants.DEFAULT_TIME_OUT));
        } catch (NacosException e) {
            LOG.error("Get data from nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}

package org.dromara.soul.plugin.sofa.subscriber;

import com.google.common.collect.Maps;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.sofa.cache.ApplicationConfigCache;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;

import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

/**
 * The sofa metadata subscribe.
 *
 * @author tydhot
 */
public class SofaMetaDataSubscriber implements MetaDataSubscriber {

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    @Override
    public void onSubscribe(final MetaData metaData) {
        if (RpcTypeEnum.SOFA.getName().equals(metaData.getRpcType())) {
            MetaData exist = META_DATA.get(metaData.getPath());
            if (Objects.isNull(exist) || Objects.isNull(ApplicationConfigCache.getInstance().get(exist.getServiceName()).refer())) {
                //第一次初始化
                ApplicationConfigCache.getInstance().initRef(metaData);
            } else {
                if (!exist.getServiceName().equals(metaData.getServiceName()) || !exist.getRpcExt().equals(metaData.getRpcExt())) {
                    //有更新
                    ApplicationConfigCache.getInstance().build(metaData);
                }
            }
            META_DATA.put(metaData.getPath(), metaData);
        }
    }

    @Override
    public void unSubscribe(final MetaData metaData) {

    }
}

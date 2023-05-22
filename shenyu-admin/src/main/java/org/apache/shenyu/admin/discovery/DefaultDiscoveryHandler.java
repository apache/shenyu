package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.spi.ExtensionLoader;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 *
 */
public class DefaultDiscoveryHandler implements DiscoveryHandler {
    // discoveryId,ShenyuDiscoveryService
    Map<String, ShenyuDiscoveryService> cache;

    @Override
    public void create(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {
        String type = discoveryDO.getType();
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        discoveryService.init(GsonUtils.getGson().fromJson(props, DiscoveryConfig.class));
        String listenerNode = discoveryDO.getListenerNode();
        String serviceList = discoveryDO.getServiceList();
        String[] split = serviceList.split(",");
        List<String> list = new ArrayList<>();
        for (String server : split) {
            list.add(listenerNode + "/" + server);
        }
        for (String key : list) {
            discoveryService.watcher(key, getDiscoveryDataChangedEventListener());
        }
    }

    @Override
    public void update(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = cache.get(discoveryDO.getId());
        List<String> removeList = getRemoveList();
        List<String> addList = getAddList();
        for (String key : removeList) {
            removePath(key);
        }
        for (String key : addList) {
            shenyuDiscoveryService.watcher(key, getDiscoveryDataChangedEventListener());
        }
    }

    @Override
    public void remove(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = cache.get(discoveryDO.getId());
        List<String> removeList = getRemoveList();
        for (String key : removeList) {
            // send remove event to gateway
        }
    }

    private void removePath(String key) {
    }


    private List<String> getRemoveList() {
        return null;
    }

    private List<String> getAddList() {
        return null;
    }

    private DiscoveryDataChangedEventListener getDiscoveryDataChangedEventListener() {
        return null;
    }


}

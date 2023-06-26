package org.apache.shenyu.plugin.sync.data.websocket.handler;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import java.util.List;

public class ProxySelectorDataHandler extends AbstractDataHandler<ProxySelectorData> {

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    public ProxySelectorDataHandler(final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers) {
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
    }

    @Override
    protected List<ProxySelectorData> convert(final String json) {
        return GsonUtils.getInstance().fromList(json, ProxySelectorData.class);
    }

    @Override
    protected void doRefresh(final List<ProxySelectorData> dataList) {
        dataList.forEach(data -> {
            proxySelectorDataSubscribers.forEach(p -> {
                p.onSubscribe(data);
            });
        });
    }

    @Override
    protected void doUpdate(final List<ProxySelectorData> dataList) {
        dataList.forEach(data -> {
            proxySelectorDataSubscribers.forEach(p -> {
                p.onSubscribe(data);
            });
        });
    }

    @Override
    protected void doDelete(final List<ProxySelectorData> dataList) {
        dataList.forEach(data -> {
            proxySelectorDataSubscribers.forEach(p -> {
                p.unSubscribe(data);
            });
        });
    }

}
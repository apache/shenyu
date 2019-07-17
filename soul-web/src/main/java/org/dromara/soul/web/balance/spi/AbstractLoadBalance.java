package org.dromara.soul.web.balance.spi;

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.web.balance.LoadBalance;

import java.util.List;

/**
 * The type Abstract load balance.
 *
 * @author xiaoyu(Myth)
 */
public abstract class AbstractLoadBalance implements LoadBalance {

    /**
     * Do select divide upstream.
     *
     * @param upstreamList the upstream list
     * @param ip           the ip
     * @return the divide upstream
     */
    protected abstract DivideUpstream doSelect(List<DivideUpstream> upstreamList, String ip);

    @Override
    public DivideUpstream select(final List<DivideUpstream> upstreamList, final String ip) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }
        if (upstreamList.size() == 1) {
            return upstreamList.get(0);
        }
        return doSelect(upstreamList, ip);
    }

}

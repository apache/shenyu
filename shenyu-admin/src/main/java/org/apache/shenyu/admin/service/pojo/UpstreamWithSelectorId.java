package org.apache.shenyu.admin.service.pojo;

import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;

/**
 * check upstream selectorId pojo
 */
public class UpstreamWithSelectorId {
    private String selectorId;

    private CommonUpstream upstream;

    /**
     * total check times.
     */
    private int zombieCheckTimes;


    public UpstreamWithSelectorId (final String selectorId, final CommonUpstream upstream) {
        this.selectorId = selectorId;
        this.upstream = upstream;
    }

    public UpstreamWithSelectorId (final String selectorId, final CommonUpstream upstream, final int zombieCheckTimes) {
        this.selectorId = selectorId;
        this.upstream = upstream;
        this.zombieCheckTimes = zombieCheckTimes;
    }

    public String getSelectorId () {
        return selectorId;
    }

    public void setSelectorId (final String selectorId) {
        this.selectorId = selectorId;
    }

    public CommonUpstream getUpstream () {
        return upstream;
    }

    public void setUpstream (final CommonUpstream upstream) {
        this.upstream = upstream;
    }

    public int getZombieCheckTimes () {
        return zombieCheckTimes;
    }

    public void setZombieCheckTimes (final int zombieCheckTimes) {
        this.zombieCheckTimes = zombieCheckTimes;
    }
}

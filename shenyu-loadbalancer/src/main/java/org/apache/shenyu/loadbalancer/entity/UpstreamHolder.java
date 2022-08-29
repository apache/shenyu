package org.apache.shenyu.loadbalancer.entity;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author tynan.liu
 * @date 2022/8/29 15:48
 **/
public class UpstreamHolder {
    private int totalWeight;
    private List<Upstream> upstreams;

    public UpstreamHolder() {
        this.totalWeight = 0;
        this.upstreams = new ArrayList<>();
    }

    public UpstreamHolder(int totalWeight, List<Upstream> upstreams) {
        this.totalWeight = totalWeight;
        this.upstreams = upstreams;
    }

    public int getTotalWeight() {
        return totalWeight;
    }

    public void setTotalWeight(int totalWeight) {
        this.totalWeight = totalWeight;
    }

    public List<Upstream> getUpstreams() {
        return upstreams;
    }

    public void setUpstreams(List<Upstream> upstreams) {
        this.upstreams = upstreams;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UpstreamHolder that = (UpstreamHolder) o;
        return totalWeight == that.totalWeight && Objects.equals(upstreams, that.upstreams);
    }

    @Override
    public int hashCode() {
        return Objects.hash(totalWeight, upstreams);
    }

    @Override
    public String toString() {
        return "UpstreamHolder{" +
                "totalWeight=" + totalWeight +
                ", upstreams=" + upstreams +
                '}';
    }

}

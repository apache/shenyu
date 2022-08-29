package org.apache.shenyu.loadbalancer.util;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.List;

/**
 * @author tynan.liu
 * @date 2022/8/29 16:33
 **/
public class WeightUtil {

    public static int calculateTotalWeight(final List<Upstream> upstreamList) {
        // total weight.
        int totalWeight = 0;
        for (Upstream upstream : upstreamList) {
            int weight = getWeight(upstream);
            // Cumulative total weight.
            totalWeight += weight;
        }
        return totalWeight;
    }

    public static int getWeight(final Upstream upstream) {
        if (!upstream.isStatus()) {
            return 0;
        }
        return getWeight(upstream.getTimestamp(), upstream.getWarmup(), upstream.getWeight());
    }

    private static int getWeight(final long timestamp, final int warmup, final int weight) {
        if (weight > 0 && timestamp > 0) {
            int uptime = (int) (System.currentTimeMillis() - timestamp);
            if (uptime > 0 && uptime < warmup) {
                return calculateWarmupWeight(uptime, warmup, weight);
            }
        }
        return weight;
    }

    private static int calculateWarmupWeight(final int uptime, final int warmup, final int weight) {
        int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
        return ww < 1 ? 1 : (Math.min(ww, weight));
    }
}

package org.dromara.soul.web.disruptor.handler;

import com.lmax.disruptor.WorkHandler;
import org.dromara.soul.web.disruptor.event.SoulDataEvent;
import org.dromara.soul.web.influxdb.service.InfluxDbService;

import java.util.concurrent.Executor;

/**
 * this is disruptor consumer.
 *
 * @author xiaoyu(Myth)
 */
public class SoulDataHandler implements WorkHandler<SoulDataEvent> {

    private Executor executor;

    private final InfluxDbService influxDbService;

    public SoulDataHandler(final Executor executor, final InfluxDbService influxDbService) {
        this.executor = executor;
        this.influxDbService = influxDbService;
    }

    @Override
    public void onEvent(final SoulDataEvent event) {
        executor.execute(() -> {
            influxDbService.writeData(event.getMonitorDO());
            event.clear();
        });
    }
}

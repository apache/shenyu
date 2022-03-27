package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.boot.CommandLineRunner;

import javax.annotation.Resource;

/**
 * TODO
 */
public abstract class AbstractDataChangedInit implements CommandLineRunner {

    @Resource
    public SyncDataService syncDataService;

    @Override
    public void run(final String... args) throws Exception {
        if (notExist()){
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }

    protected abstract boolean notExist();
}

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.enums.DataEventTypeEnum;

import javax.annotation.Resource;

/**
 * AbstractDataChangedInit.
 */
public abstract class AbstractDataChangedInit implements DataChangedInit {

    /**
     * SyncDataService, sync all data.
     */
    @Resource
    public SyncDataService syncDataService;

    @Override
    public void run(final String... args) throws Exception {
        if (notExist()){
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }

    /**
     * check exist.
     *
     * @return boolean.
     */
    protected abstract boolean notExist();
}

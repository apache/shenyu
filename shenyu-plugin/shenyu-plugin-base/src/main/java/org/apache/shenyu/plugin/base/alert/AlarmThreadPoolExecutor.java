package org.apache.shenyu.plugin.base.alert;

import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * ShenyuThreadPoolExecutor for alarm sender send async data
 */
public class AlarmThreadPoolExecutor {
    
    private static ShenyuThreadPoolExecutor threadPoolExecutor;
    
    
    private AlarmThreadPoolExecutor() {
        initWorkExecutor();
    }
    
    private void initWorkExecutor() {
        threadPoolExecutor =  new ShenyuThreadPoolExecutor(10, 100, 60000L,
                TimeUnit.MILLISECONDS, new MemorySafeTaskQueue<>(Constants.THE_256_MB),
                ShenyuThreadFactory.create("alarm sender", true),
                new ThreadPoolExecutor.AbortPolicy());
    }
    
    /**
     * execute alarm runnable task
     * @param runnable task
     */
    public void execute(Runnable runnable) {
        threadPoolExecutor.execute(runnable);
    }
    
    /**
     * get AlarmThreadPoolExecutor single instance
     * @return AlarmThreadPoolExecutor instance
     */
    public static AlarmThreadPoolExecutor getInstance() {
        return SingleInstance.INSTANCE;
    }
    
    /**
     * single instance for AlarmThreadPoolExecutor
     */
    private static class SingleInstance {
        private static final AlarmThreadPoolExecutor INSTANCE = new AlarmThreadPoolExecutor();
    }
}

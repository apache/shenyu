package org.apache.shenyu.plugin.aliyun.sls;

import com.aliyun.openservices.log.Client;
import com.aliyun.openservices.log.common.LogItem;
import com.aliyun.openservices.log.common.LogStore;
import com.aliyun.openservices.log.exception.LogException;
import com.aliyun.openservices.log.response.CreateLogStoreResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant;
import org.apache.shenyu.plugin.aliyun.sls.entity.ShenyuRequestLog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

public class AliyunSlsLogCollectClient implements LogConsumeClient {

    private static final Logger LOG = LoggerFactory.getLogger(AliyunSlsLogCollectClient.class);

    private Client client;

    private String projectName;

    private String logStore;

    private String topic;

    private final AtomicBoolean isStarted = new AtomicBoolean(false);

    public void initClient(final Properties props) {
        if (MapUtils.isEmpty(props)) {
            LOG.error("RocketMQ props is empty. failed init RocketMQ producer");
            return;
        }
        if (isStarted.get()) {
            close();
        }
        String accessId = props.getProperty(LoggingConstant.ACCESS_ID);
        String accessKey = props.getProperty(LoggingConstant.ACCESS_KEY);
        String host = props.getProperty(LoggingConstant.HOST);
        client = new Client(host, accessId, accessKey);

        // create LogStore
        projectName = props.getProperty(LoggingConstant.PROJECT_NAME);
        logStore = props.getProperty(LoggingConstant.LOG_STORE);
        topic = props.getProperty(LoggingConstant.TOPIC);
        int ttlInDay = Integer.parseInt(props.getProperty(LoggingConstant.TTL_IN_DAY));
        int shardCount = Integer.parseInt(props.getProperty(LoggingConstant.SHARD_COUNT));
        LogStore store = new LogStore(logStore, ttlInDay, shardCount);
        try {
            isStarted.set(true);
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
            CreateLogStoreResponse res = client.CreateLogStore(projectName, store);

        } catch (LogException e) {
            LOG.error("error code:{}, error message:{}, error requestId:{}", e.GetErrorCode(), e.GetErrorMessage(), e.getRequestId());
        }
    }

    @Override
    public void consume(List<ShenyuRequestLog> logs) throws Exception {
        if (CollectionUtils.isEmpty(logs) || !isStarted.get()) {
            return;
        }

        logs.forEach(log -> {
            List<LogItem> logGroup = new ArrayList<LogItem>();
            LogItem logItem = new LogItem((int) (System.currentTimeMillis() / 1000));
            logItem.PushBack("level", "info");
            logItem.PushBack("name", log.getMethod());
            logItem.PushBack("message", GsonUtils.getGson().toJson(log));
            logGroup.add(logItem);
            try {
                client.PutLogs(projectName, logStore, topic, logGroup, "");
            } catch (LogException e) {
                LOG.error("error code :{}, error message :{}, error message :{}", e.GetErrorCode(), e.GetErrorMessage(), e.getRequestId());
            }
        });
    }

    @Override
    public void close() {
        if (client != null && isStarted.get()) {
            client.shutdown();
            isStarted.set(false);
        }
    }
}

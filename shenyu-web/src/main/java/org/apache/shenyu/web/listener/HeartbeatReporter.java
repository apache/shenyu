package org.apache.shenyu.web.listener;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.netty.handler.codec.Headers;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.servlet.context.ServletWebServerInitializedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.util.Assert;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.IOException;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HeartbeatReporter implements ApplicationListener<ServletWebServerInitializedEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatReporter.class);

    private ScheduledThreadPoolExecutor executor;


    public HeartbeatReporter(ScheduledThreadPoolExecutor executor) {
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
    }

    @Override
    public void onApplicationEvent(ServletWebServerInitializedEvent event) {

        int port = event.getWebServer().getPort();
        LOG.info("Web server initialized on port {}, starting heartbeat reporter", port);

//        // 初始化本地IP
//        localIp = getLocalIp();
//        log.info("Detected local IP: {}", localIp);

        // 启动心跳任务
//        executor.scheduleAtFixedRate(this::report,
//                0,
//                5,
//                TimeUnit.SECONDS
//        );
    }
//    /**
//     * Get the host.
//     *
//     * @return the host
//     */
//    public String getHost() {
//        return IpUtils.isCompleteHost(this.host) ? this.host
//                : IpUtils.getHost(this.host);
//    }
//    private void report() {
//        MultiValueMap<String, String> params = new LinkedMultiValueMap<>(8);
//        params.put("namespaceId", Collections.singletonList("1"));
//        LOG.debug("listener params: [{}]", params);
//        Headers headers = new Headers.Builder()
//                .add(Constants.X_ACCESS_TOKEN, accessTokenManager.getAccessToken())
//                .add("Content-Type", "application/x-www-form-urlencoded")
//                .add("X-Real-PORT", port + "")
//                .add(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO, SystemInfoUtils.getSystemInfo())
//                .build();
//        String listenerUrl = server + Constants.SHENYU_ADMIN_PATH_CONFIGS_LISTENER;
//        String uri = UriComponentsBuilder.fromHttpUrl(listenerUrl).queryParams(params).build(true).toUriString();
//        Request request = new Request.Builder()
//                .url(uri)
//                .headers(headers)
//                .post(RequestBody.create("", null))
//                .build();
//
//        JsonArray groupJson;
//        try (Response response = okHttpClient.newCall(request).execute()) {
//            if (!response.isSuccessful()) {
//                String message = String.format("listener configs fail, server:[%s], http status code[%s]", server, response.code());
//                throw new ShenyuException(message);
//            }
//            ResponseBody responseBody = response.body();
//            Assert.notNull(responseBody, "Resolve response body failed.");
//            String json = responseBody.string();
//            LOG.info("listener result: [{}]", json);
//            JsonObject responseFromServer = GsonUtils.getGson().fromJson(json, JsonObject.class);
//            JsonElement element = responseFromServer.get("data");
//            if (element.isJsonNull()) {
//                return;
//            }
//            groupJson = responseFromServer.getAsJsonArray("data");
//        } catch (IOException e) {
//            String message = String.format("listener configs fail, server:[%s], %s", server, e.getMessage());
//            throw new ShenyuException(message, e);
//        }
//    }
//
//
//    @EventListener(ContextClosedEvent.class)
//    public void onShutdown() {
//        scheduler.shutdown();
//        try {
//            if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
//                scheduler.shutdownNow();
//            }
//        } catch (InterruptedException e) {
//            Thread.currentThread().interrupt();
//        }
//        log.info("Heartbeat reporter shutdown complete");
//    }
}
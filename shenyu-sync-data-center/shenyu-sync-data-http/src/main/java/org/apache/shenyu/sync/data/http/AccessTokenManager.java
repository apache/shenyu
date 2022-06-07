package org.apache.shenyu.sync.data.http;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * AccessTokenManager.
 */
public class AccessTokenManager {

    public static final Logger LOG = LoggerFactory.getLogger(AccessTokenManager.class);

    /**
     * the access token.
     */
    private volatile String accessToken;

    /**
     * TTL of token in seconds.
     */
    private long tokenExpiredTime;

    /**
     * Last timestamp refresh security info from server.
     */
    private long lastRefreshTime;

    /**
     * Time window to refresh security info in seconds.
     */
    private long tokenRefreshWindow;

    private final RestTemplate restTemplate;

    private final HttpConfig httpConfig;

    private final ScheduledExecutorService executorService;

    /**
     * Construct.
     *
     * @param restTemplate the rest template.
     * @param httpConfig   the config.
     */
    public AccessTokenManager(final RestTemplate restTemplate, final HttpConfig httpConfig) {
        this.restTemplate = restTemplate;
        this.httpConfig = httpConfig;
        this.executorService = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("http-long-polling-client-token-refresh", true));
        this.start(Lists.newArrayList(Splitter.on(",").split(httpConfig.getUrl())));
    }

    public void login(final List<String> servers){
        if ((System.currentTimeMillis() - lastRefreshTime) < (tokenExpiredTime - tokenRefreshWindow)) {
            return;
        }
        for (String server : servers) {
            if (this.doLogin(server)) {
                this.lastRefreshTime = System.currentTimeMillis();
                return;
            }
        }
    }

    private Boolean doLogin(final String server) {
        String param = Constants.LOGIN_NAME + "=" + httpConfig.getUsername() + "&" + Constants.PASS_WORD + "=" + httpConfig.getPassword();
        String url = String.join("?", server + Constants.LOGIN_PATH, param);
        try {
            String result = this.restTemplate.getForObject(url, String.class);
            Map<String, Object> resultMap = GsonUtils.getInstance().convertToMap(result);
            if (!String.valueOf(CommonErrorCode.SUCCESSFUL).equals(String.valueOf(resultMap.get(Constants.ADMIN_RESULT_CODE)))) {
                LOG.warn(String.format("get token from server : [%s] error", server));
                return false;
            }
            String tokenJson = GsonUtils.getInstance().toJson(resultMap.get(Constants.ADMIN_RESULT_DATA));
            LOG.info("login success: {} ", tokenJson);
            Map<String, Object> tokenMap = GsonUtils.getInstance().convertToMap(tokenJson);
            this.accessToken =  (String) tokenMap.get(Constants.ADMIN_RESULT_TOKEN);
            this.tokenExpiredTime = (long)tokenMap.get(Constants.ADMIN_RESULT_EXPIRED_TIME);
            this.tokenRefreshWindow = this.tokenExpiredTime / 10;
            return true;
        } catch (RestClientException e) {
            LOG.error(String.format("get token from server : [%s] error", server), e);
            return false;
        }
    }

    private void start(final List<String> servers){
        this.login(servers);
        this.executorService.scheduleWithFixedDelay(() -> this.login(servers), 0, 5000, TimeUnit.MILLISECONDS);
    }

    public String getAccessToken() {
        return accessToken;
    }
}

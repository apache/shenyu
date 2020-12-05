package org.dromara.soul.plugin.sign.service;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.AuthPathData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.plugin.api.SignService;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.dromara.soul.plugin.base.cache.BaseDataCache;
import org.dromara.soul.plugin.sign.cache.SignAuthDataCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.Collections;
import java.util.Map;

/**
 * @author Phoenix Luo
 * @version 2020/12/5
 **/
@RunWith(MockitoJUnitRunner.class)
public class DefaultSignServiceTest {
    
    private SignService signService;
    
    private ServerWebExchange exchange;
    
    private String appKey = "D1DFC83F3BC64FABB89DFBD54E5A28C8";
    
    private String secretKey = "692C479F98C841FCBEB444B7CA775F63";
    
    private SoulContext passed;
    
    @Value("${soul.sign.delay}")
    private int delay;
    
    @Before
    public void setup() {
        this.signService = new DefaultSignService();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        
        String path = "/test-api/demo/test";
        PluginData signData = new PluginData();
        signData.setId("1");
        signData.setName(PluginEnum.SIGN.getName());
        signData.setEnabled(true);
        signData.setRole(1);
        BaseDataCache.getInstance().cachePluginData(signData);
        
        AppAuthData authData = new AppAuthData();
        authData.setAppKey(appKey);
        authData.setAppSecret(secretKey);
        authData.setEnabled(true);
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName("test-api");
        authPathData.setPath(path);
        authPathData.setEnabled(true);
        authData.setPathDataList(Lists.newArrayList(authPathData));
        SignAuthDataCache.getInstance().cacheAuthData(authData);
        
        this.passed = new SoulContext();
        this.passed.setModule("/test-api");
        this.passed.setMethod("/demo/test");
        this.passed.setRpcType("springCloud");
        this.passed.setHttpMethod("GET");
        this.passed.setPath(path);
        String timeStamp = String.valueOf(System.currentTimeMillis());
        this.passed.setTimestamp(timeStamp);
        this.passed.setSign(buildSign(secretKey, timeStamp, this.passed.getPath()));
        this.passed.setAppKey(appKey);
        this.passed.setContextPath("/test-api");
        this.passed.setRealUrl("/demo/test");
    }
    
    @Test
    public void normalTest() {
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert ret.getLeft() : "sign正向测试未通过！";
    }
    
    @Test
    public void nullTimestampTest() {
        this.passed.setTimestamp(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_PARAMS_ERROR.equals(ret.getRight()) : "空时间戳测试未通过！";
        
    }
    
    @Test
    public void nullSignTest() {
        this.passed.setSign(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_PARAMS_ERROR.equals(ret.getRight()) : "空sign测试未通过！";
        
    }
    
    @Test
    public void nullAppKeyTest() {
        this.passed.setAppKey(null);
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_PARAMS_ERROR.equals(ret.getRight()) : "空appKey测试未通过！";
        
    }
    
    @Test
    public void overdueTest() {
        Long errorTimestamp = Long.valueOf(this.passed.getTimestamp()) - Long.valueOf((delay + 1) * 1000 * 60);
        this.passed.setTimestamp(errorTimestamp.toString());
        this.passed.setSign(buildSign(secretKey, this.passed.getTimestamp(), this.passed.getPath()));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && String.format(SoulResultEnum.SING_TIME_IS_TIMEOUT.getMsg(), delay).equals(ret.getRight()) : "超时测试未通过！";
        
    }
    
    @Test
    public void errorAppKeyTest() {
        this.passed.setAppKey("errorKey");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_APP_KEY_IS_NOT_EXIST.equals(ret.getRight()) : "错误AppKey测试未通过！";
    }
    
    @Test
    public void emptyAuthPath() {
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        AppAuthData authData = SignAuthDataCache.getInstance().obtainAuthData(appKey);
        authData.setPathDataList(Collections.emptyList());
        SignAuthDataCache.getInstance().cacheAuthData(authData);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_PATH_NOT_EXIST.equals(ret.getRight()) : "配置路径为空测试未通过";
    }
    
    @Test
    public void errorAuthPath() {
        this.passed.setPath("errorPath");
        this.passed.setSign(buildSign(secretKey, this.passed.getTimestamp(), this.passed.getPath()));
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_PATH_NOT_EXIST.equals(ret.getRight()) : "配置路径未配置测试未通过";
    }
    
    @Test
    public void errorSign() {
        this.passed.setSign("errorSign");
        this.exchange.getAttributes().put(Constants.CONTEXT, this.passed);
        
        Pair<Boolean, String> ret = this.signService.signVerify(this.exchange);
        assert !ret.getLeft() && Constants.SIGN_VALUE_IS_ERROR.equals(ret.getRight()) : "错误Sing测试未通过";
    }
    
    
    private String buildSign(String signKey, String timeStamp, String path) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(3);
        map.put(Constants.TIMESTAMP, timeStamp);
        map.put(Constants.PATH, path);
        map.put(Constants.VERSION, "1.0.0");
        return SignUtils.generateSign(signKey, map);
    }
}

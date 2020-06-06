package org.dromara.soul.client.apache.dubbo;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.dubbo.config.spring.ServiceBean;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.dubbo.common.annotation.SoulDubboClient;
import org.dromara.soul.client.dubbo.common.config.DubboConfig;
import org.dromara.soul.client.dubbo.common.dto.MetaDataDTO;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

/**
 * The Apache Dubbo ServiceBean PostProcessor.
 *
 * @author xiaoyu
 */
@Slf4j
public class ApacheDubboServiceBeanPostProcessor implements BeanPostProcessor, ApplicationListener<ContextRefreshedEvent> {
    
    private DubboConfig dubboConfig;
    
    private ExecutorService executorService = Executors.newSingleThreadExecutor();
    
    private final String url;
    
    public ApacheDubboServiceBeanPostProcessor(final DubboConfig dubboConfig) {
        String contextPath = dubboConfig.getContextPath();
        String adminUrl = dubboConfig.getAdminUrl();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)) {
            throw new RuntimeException("apache dubbo client must config the contextPath, adminUrl");
        }
        this.dubboConfig = dubboConfig;
        url = dubboConfig.getAdminUrl() + "/soul-client/dubbo-register";
    }
    
    @Override
    public Object postProcessBeforeInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean instanceof ServiceBean) {
            executorService.execute(() -> handler((ServiceBean) bean));
        }
        return bean;
    }
    
    private void handler(final ServiceBean serviceBean) {
        Class<?> clazz = serviceBean.getRef().getClass();
        if (ClassUtils.isCglibProxyClass(clazz)) {
            String superClassName = clazz.getGenericSuperclass().getTypeName();
            try {
                clazz = Class.forName(superClassName);
            } catch (ClassNotFoundException e) {
                log.error(String.format("class not found: %s", superClassName));
                return;
            }
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulDubboClient soulDubboClient = method.getAnnotation(SoulDubboClient.class);
            if (Objects.nonNull(soulDubboClient)) {
                post(buildJsonParams(serviceBean, soulDubboClient, method));
            }
        }
    }
    
    private String buildJsonParams(final ServiceBean serviceBean, final SoulDubboClient soulDubboClient, final Method method) {
        String appName = dubboConfig.getAppName();
        if (appName == null || "".equals(appName)) {
            appName = serviceBean.getApplication().getName();
        }
        String path = dubboConfig.getContextPath() + soulDubboClient.path();
        String desc = soulDubboClient.desc();
        String serviceName = serviceBean.getInterface();
        String configRuleName = soulDubboClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(dubboConfig.getContextPath())
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExt(serviceBean))
                .rpcType("dubbo")
                .enabled(soulDubboClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(metaDataDTO);
        
    }
    
    private String buildRpcExt(final ServiceBean serviceBean) {
        MetaDataDTO.RpcExt build = MetaDataDTO.RpcExt.builder()
                .group(serviceBean.getGroup())
                .version(serviceBean.getVersion())
                .loadbalance(serviceBean.getLoadbalance())
                .retries(serviceBean.getRetries())
                .timeout(serviceBean.getTimeout())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(build);
        
    }
    
    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("dubbo client register success :{} " + json);
            } else {
                log.error("dubbo client register error :{} " + json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        executorService.shutdown();
    }
}

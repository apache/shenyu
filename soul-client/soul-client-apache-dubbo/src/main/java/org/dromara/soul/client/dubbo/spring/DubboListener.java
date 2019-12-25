package org.dromara.soul.client.dubbo.spring;

import lombok.extern.slf4j.Slf4j;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.dubbo.config.spring.context.event.ServiceBeanExportedEvent;
import org.dromara.soul.client.common.dto.MetaDataDTO;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.client.dubbo.config.DubboConfig;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

/**
 * The type Dubbo listener.
 *
 * @author xiaoyu
 */
@Slf4j
public class DubboListener implements ApplicationListener {

    private DubboConfig dubboConfig;

    private ExecutorService executorService = Executors.newSingleThreadExecutor();

    private final String url;

    public DubboListener(final DubboConfig dubboConfig) {
        this.dubboConfig = dubboConfig;
        url = dubboConfig.getAdminUrl() + "/meta-data/register";
    }

    @Override
    public void onApplicationEvent(final ApplicationEvent event) {
        if (event instanceof ServiceBeanExportedEvent) {
            executorService.execute(() -> handler((ServiceBeanExportedEvent) event));
        }
    }

    private void handler(final ServiceBeanExportedEvent event) {
        ServiceBean serviceBean = event.getServiceBean();
        Class<?> clazz = serviceBean.getRef().getClass();
        if(ClassUtils.isCglibProxyClass(clazz)){
            String superClassName = clazz.getGenericSuperclass().getTypeName();
            try {
                clazz = Class.forName(superClassName);
            } catch (ClassNotFoundException e) {
                log.error(String.format("class not found: %s", superClassName) );
                return;
            }
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulClient soulClient = method.getAnnotation(SoulClient.class);
            if (Objects.nonNull(soulClient)) {
                String contextPath = dubboConfig.getContextPath();
                String adminUrl = dubboConfig.getAdminUrl();
                if (contextPath == null || "".equals(contextPath)
                        || adminUrl == null || "".equals(adminUrl)) {
                    log.error(".......dubbo client must config context-path and soul admin url......");
                    return;
                }
                post(buildJsonParams(serviceBean, soulClient, method));
            }
        }
    }

    private String buildJsonParams(final ServiceBean serviceBean, final SoulClient soulClient, final Method method) {
        String appName = dubboConfig.getAppName();
        if (appName == null || "".equals(appName)) {
            appName = serviceBean.getApplication().getName();
        }
        String path = dubboConfig.getContextPath() + soulClient.path();
        String desc = soulClient.desc();
        String serviceName = serviceBean.getInterface();
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .path(path)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExt(serviceBean))
                .rpcType("dubbo")
                .enabled(soulClient.enabled())
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

}

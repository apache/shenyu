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

    public DubboListener(final DubboConfig dubboConfig) {
        this.dubboConfig = dubboConfig;
    }

    @Override
    public void onApplicationEvent(ApplicationEvent event) {
        if (event instanceof ServiceBeanExportedEvent) {
            executorService.execute(() -> handler((ServiceBeanExportedEvent) event));
        }
    }

    private void handler(ServiceBeanExportedEvent event) {
        ServiceBean serviceBean = event.getServiceBean();
        Class<?> clazz = serviceBean.getRef().getClass();
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulClient soulClient = method.getAnnotation(SoulClient.class);
            if (Objects.nonNull(soulClient)) {
                if (dubboConfig.getContextPath() == null
                        || dubboConfig.getContextPath().equals("")) {
                    log.error("请你先配置contextPath :{}", serviceBean.getApplication().getName());
                    return;
                }
                post(buildJsonParams(serviceBean, soulClient, method));
            }
        }
    }

    private String buildJsonParams(ServiceBean serviceBean, SoulClient soulClient, Method method) {
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

    private String buildRpcExt(ServiceBean serviceBean) {
        MetaDataDTO.RpcExt build = MetaDataDTO.RpcExt.builder()
                .group(serviceBean.getGroup())
                .version(serviceBean.getVersion())
                .loadbalance(serviceBean.getLoadbalance())
                .retries(serviceBean.getRetries())
                .timeout(serviceBean.getTimeout())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(build);

    }

    private void post(String json) {
        try {
            String result = OkHttpTools.getInstance().post(dubboConfig.getUrl(), json);
            if (Objects.equals(result, "success")) {
                log.info("dubbo client register success :{} " + json);
            } else {
                log.error("dubbo client register error :{} " + json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", dubboConfig.getUrl() + ":" + json);
        }
    }

}

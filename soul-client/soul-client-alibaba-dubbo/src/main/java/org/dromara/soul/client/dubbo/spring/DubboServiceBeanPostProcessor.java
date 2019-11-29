package org.dromara.soul.client.dubbo.spring;

import com.alibaba.dubbo.config.spring.ServiceBean;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.client.common.dto.MetaDataDTO;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.dubbo.config.DubboConfig;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.util.ReflectionUtils;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

/**
 * The DubboServiceBeanPostProcessor.
 *
 * @author xiaoyu
 */
@Slf4j
public class DubboServiceBeanPostProcessor implements BeanPostProcessor {

    private DubboConfig dubboConfig;

    private ExecutorService executorService = Executors.newSingleThreadExecutor();

    private final String url;

    public DubboServiceBeanPostProcessor(final DubboConfig dubboConfig) {
        this.dubboConfig = dubboConfig;
        url = dubboConfig.getAdminUrl() + "/meta-data/register";
    }

    @Override
    public Object postProcessBeforeInitialization(final Object bean, final String beanName) throws BeansException {
        return bean;
    }

    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean instanceof ServiceBean) {
            executorService.execute(() -> handler((ServiceBean) bean));
        }
        return bean;
    }

    private void handler(final ServiceBean serviceBean) {
        Class<?> clazz = serviceBean.getRef().getClass();
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulClient soulClient = method.getAnnotation(SoulClient.class);
            if (Objects.nonNull(soulClient)) {
                String contextPath = dubboConfig.getContextPath();
                String adminUrl = dubboConfig.getAdminUrl();
                if (contextPath == null || "".equals(contextPath)
                        || adminUrl == null || "".equals(adminUrl)) {
                    log.error("........dubbo client must config context-path and adminUrl.........");
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

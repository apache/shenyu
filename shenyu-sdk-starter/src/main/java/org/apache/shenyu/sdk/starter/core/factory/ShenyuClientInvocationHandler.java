package org.apache.shenyu.sdk.starter.core.factory;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.ShenyuClient;
import org.apache.shenyu.sdk.starter.core.ShenyuClientFactoryBean;
import org.apache.shenyu.sdk.starter.core.ShenyuHttpClient;
import org.apache.shenyu.sdk.starter.core.SpringMvcContract;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ObjectUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuClientInvocationHandler implements InvocationHandler {

    private final Map<Method, ShenyuClientMethodHandler> methodHandlerMap = new ConcurrentHashMap<>();

    private final ApplicationContext applicationContext;

    private final ShenyuHttpClient shenyuHttpClient;

    private final SpringMvcContract springMvcContract;

    private final ShenyuClientFactoryBean shenyuClientFactoryBean;

    private final Collection<RequestPostProcessor> requestPostProcessors;

    public ShenyuClientInvocationHandler(final Class<?> apiClass, final ApplicationContext applicationContext, ShenyuClientFactoryBean shenyuClientFactoryBean) {
        this.applicationContext = applicationContext;
        this.shenyuClientFactoryBean = shenyuClientFactoryBean;
        ShenyuClient shenyuClient = apiClass.getAnnotation(ShenyuClient.class);
        if (shenyuClient == null) {
            throw new NullPointerException("NOT FOUND @ShenyuClient BY " + apiClass.getName());
        }
        this.shenyuHttpClient = applicationContext.getBean(ShenyuHttpClient.class);
        this.springMvcContract = applicationContext.getBean(SpringMvcContract.class);
        final Map<String, RequestPostProcessor> beansOfType = applicationContext.getBeansOfType(RequestPostProcessor.class);
        this.requestPostProcessors = beansOfType.values();
        buildMethodHandlerMap(apiClass, shenyuHttpClient, shenyuClient);
    }

    private void buildMethodHandlerMap(final Class<?> apiClass, final ShenyuHttpClient shenyuHttpClient, final ShenyuClient shenyuClient) {
        Method[] methods = apiClass.getMethods();
        for (Method method : methods) {
            if (method.getDeclaringClass() == Object.class ||
                    (method.getModifiers() & Modifier.STATIC) != 0 ||
                    isDefault(method)) {
                continue;
            }
            final RequestTemplate requestTemplate = springMvcContract.parseRequestTemplate(method);
            requestTemplate.setUrl(shenyuClientFactoryBean.getUrl());
            requestTemplate.setParamMetadataList(this.analysisParamMetadata(method));
            methodHandlerMap.put(method, new ShenyuClientMethodHandler(shenyuClient, shenyuHttpClient, method, requestTemplate, requestPostProcessors));
        }
    }

    /**
     * Identifies a method as a default instance method.
     */
    public static boolean isDefault(final Method method) {
        // Default methods are public non-abstract, non-synthetic, and non-static instance methods
        // declared in an interface.
        // method.isDefault() is not sufficient for our usage as it does not check
        // for synthetic methods. As a result, it picks up overridden methods as well as actual default
        // methods.
        final int SYNTHETIC = 0x00001000;
        return ((method.getModifiers()
                & (Modifier.ABSTRACT | Modifier.PUBLIC | Modifier.STATIC | SYNTHETIC)) == Modifier.PUBLIC)
                && method.getDeclaringClass().isInterface();
    }

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        ShenyuClientMethodHandler handler = methodHandlerMap.get(method);
        if (ObjectUtils.isEmpty(handler)) {
            return handler.invoke(args);
        }
        return null;
    }


    private List<RequestTemplate.ParamMetadata> analysisParamMetadata(Method method) {
        List<RequestTemplate.ParamMetadata> params = new ArrayList<>();
        Parameter[] parameters = method.getParameters();
        if (parameters != null && parameters.length > 0) {
            for (int index = 0; index < parameters.length; index++) {
                Annotation[] annotations = parameters[index].getAnnotations();
                if (annotations == null || annotations.length == 0) {
                    continue;
                }
                RequestTemplate.ParamMetadata paramMetadata = new RequestTemplate.ParamMetadata();
                paramMetadata.setParamAnnotations(annotations);
                paramMetadata.setParamIndexOnMethod(index);
                paramMetadata.setParamType(parameters[index].getType());
                paramMetadata.setPrimitive(parameters[index].getType().isPrimitive());
                params.add(paramMetadata);
            }
        }
        return params;
    }

}
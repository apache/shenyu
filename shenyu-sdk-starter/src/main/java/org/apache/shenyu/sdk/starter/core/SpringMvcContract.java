package org.apache.shenyu.sdk.starter.core;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;

import static org.springframework.core.annotation.AnnotatedElementUtils.findMergedAnnotation;

public class SpringMvcContract implements ResourceLoaderAware {


    private static final String ACCEPT = "Accept";

    private static final String CONTENT_TYPE = "Content-Type";

    private ResourceLoader resourceLoader = new DefaultResourceLoader();

    public RequestTemplate parseRequestTemplate(final Method method) {
        final RequestTemplate requestTemplate = new RequestTemplate();
        for (final Annotation methodAnnotation : method.getAnnotations()) {
            this.processAnnotationOnMethod(requestTemplate, methodAnnotation, method);
        }
        return requestTemplate;
    }

    protected void processAnnotationOnMethod(final RequestTemplate requestTemplate, final Annotation methodAnnotation, final Method method) {

        if (!RequestMapping.class.isInstance(methodAnnotation)
                && !methodAnnotation.annotationType().isAnnotationPresent(RequestMapping.class)) {
            return;
        }

        RequestMapping methodMapping = findMergedAnnotation(method, RequestMapping.class);
        // HTTP Method
        RequestMethod[] methods = methodMapping.method();
        if (methods.length == 0) {
            methods = new RequestMethod[] { RequestMethod.GET };
        }
        checkOne(method, methods, "method");

        requestTemplate.setMethod(ShenyuRequest.HttpMethod.valueOf(methods[0].name()));

        // path
        checkAtMostOne(method, methodMapping.value(), "value");
        if (methodMapping.value().length > 0) {
            String pathValue = methodMapping.value()[0];
            if (pathValue != null && !pathValue.isEmpty()) {
                pathValue = resolve(pathValue);
                // Append path from @RequestMapping if value is present on method
                if (!pathValue.startsWith("/") && !requestTemplate.getPath().endsWith("/")) {
                    pathValue = "/" + pathValue;
                }
                requestTemplate.setPath(pathValue);
            }
        }

        // produces
        parseProduces(requestTemplate, methodMapping);

        // consumes
        parseConsumes(requestTemplate, methodMapping);

        // headers
        parseHeaders(requestTemplate, methodMapping);
    }

    private void parseProduces(RequestTemplate requestTemplate, RequestMapping annotation) {
        String[] serverProduces = annotation.produces();
        String clientAccepts = serverProduces.length == 0 ? null : serverProduces[0].isEmpty() ? null : serverProduces[0];
        if (clientAccepts != null) {
            requestTemplate.getHeaders().put(ACCEPT, Collections.singleton(clientAccepts));
        }
    }

    private void parseConsumes(RequestTemplate requestTemplate, RequestMapping annotation) {
        String[] serverConsumes = annotation.consumes();
        String clientProduces = serverConsumes.length == 0 ? null : serverConsumes[0].isEmpty() ? null : serverConsumes[0];
        if (clientProduces != null) {
            requestTemplate.getHeaders().put(CONTENT_TYPE, Collections.singleton(clientProduces));
        }
    }

    private void parseHeaders(RequestTemplate requestTemplate, RequestMapping annotation) {
        // TODO: only supports one header value per key
        if (annotation.headers().length > 0) {
            for (String header : annotation.headers()) {
                int index = header.indexOf('=');
                if (!header.contains("!=") && index >= 0) {
                    requestTemplate.getHeaders().put(resolve(header.substring(0, index)),
                            Collections.singleton(resolve(header.substring(index + 1).trim())));
                }
            }
        }
    }

    private String resolve(final String value) {
        if (StringUtils.hasText(value) && resourceLoader instanceof ConfigurableApplicationContext) {
            return ((ConfigurableApplicationContext) resourceLoader).getEnvironment().resolvePlaceholders(value);
        }
        return value;
    }

    private void checkOne(Method method, Object[] values, String fieldName) {
        Assert.state(values != null && values.length == 1,
                String.format("Method %s can only contain 1 %s field. Found: %s",
                        method.getName(), fieldName, values == null ? null : Arrays.asList(values)));
    }

    private void checkAtMostOne(Method method, Object[] values, String fieldName) {
        Assert.state(values != null && (values.length == 0 || values.length == 1),
                String.format("Method %s can only contain at most 1 %s field. Found: %s",
                        method.getName(), fieldName, Arrays.asList(values)));
    }

    @Override
    public void setResourceLoader(final ResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
    }
}

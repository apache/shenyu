/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.sdk.feign;

import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.annotation.AnnotatedGenericBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.BeanExpressionContext;
import org.springframework.beans.factory.config.BeanExpressionResolver;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.cloud.openfeign.FeignClientFactoryBean;
import org.springframework.cloud.openfeign.FeignClientSpecification;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.ResourceLoaderAware;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * ShenyuClientsRegistrar.
 */
public class ShenyuClientsRegistrar implements ImportBeanDefinitionRegistrar, ResourceLoaderAware, EnvironmentAware {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientsRegistrar.class);

    private ResourceLoader resourceLoader;

    private Environment environment;

    /**
     * Instantiates a new Shenyu clients registrar.
     */
    ShenyuClientsRegistrar() {
    }

    /**
     * Validate fallback.
     * @param clazz the clazz
     */
    static void validateFallback(final Class clazz) {
        Assert.isTrue(!clazz.isInterface(), "Fallback class must implement the interface annotated by @ShenyuClient");
    }

    /**
     * Validate fallback factory.
     * @param clazz the clazz
     */
    static void validateFallbackFactory(final Class clazz) {
        Assert.isTrue(!clazz.isInterface(), "Fallback factory must produce instances " + "of fallback classes that implement the interface annotated by @ShenyuClient");
    }

    @Override
    public void setResourceLoader(final ResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
    }

    @Override
    public void registerBeanDefinitions(final AnnotationMetadata metadata, final BeanDefinitionRegistry registry) {
        registerDefaultConfiguration(metadata, registry);
        registerShenyuClients(metadata, registry);
    }

    private void registerDefaultConfiguration(final AnnotationMetadata metadata, final BeanDefinitionRegistry registry) {
        Map<String, Object> defaultAttrs = metadata.getAnnotationAttributes(EnableShenyuClients.class.getName(), true);

        if (MapUtils.isNotEmpty(defaultAttrs) && defaultAttrs.containsKey("defaultConfiguration")) {
            String name;
            if (metadata.hasEnclosingClass()) {
                name = "default." + metadata.getEnclosingClassName();
            } else {
                name = "default." + metadata.getClassName();
            }
            LOG.info("registerDefaultConfiguration name:{}", name);
            registerClientConfiguration(registry, name, defaultAttrs.get("defaultConfiguration"));
        }
    }

    /**
     * Register shenyu clients.
     * @param metadata the metadata
     * @param registry the registry
     */
    public void registerShenyuClients(final AnnotationMetadata metadata, final BeanDefinitionRegistry registry) {
        Set<BeanDefinition> candidateComponents = new LinkedHashSet<>();
        Map<String, Object> attrs = metadata.getAnnotationAttributes(EnableShenyuClients.class.getName());
        final Class<?>[] clients = Objects.isNull(attrs) ? null : (Class<?>[]) attrs.get("clients");
        LOG.info("clients:{}", JsonUtils.toJson(clients));
        if (Objects.isNull(clients) || clients.length == 0) {
            ClassPathScanningCandidateComponentProvider scanner = getScanner();
            scanner.setResourceLoader(this.resourceLoader);
            scanner.addIncludeFilter(new AnnotationTypeFilter(ShenyuClient.class));
            Set<String> basePackages = getBasePackages(metadata);
            LOG.info("basePackages:{}", JsonUtils.toJson(basePackages));
            for (String basePackage : basePackages) {
                candidateComponents.addAll(scanner.findCandidateComponents(basePackage));
            }
        } else {
            for (Class<?> clazz : clients) {
                candidateComponents.add(new AnnotatedGenericBeanDefinition(clazz));
            }
        }

        for (BeanDefinition candidateComponent : candidateComponents) {
            if (candidateComponent instanceof AnnotatedBeanDefinition) {
                // verify annotated class is an interface
                AnnotatedBeanDefinition beanDefinition = (AnnotatedBeanDefinition) candidateComponent;
                AnnotationMetadata annotationMetadata = beanDefinition.getMetadata();
                Assert.isTrue(annotationMetadata.isInterface(), "@ShenyuClient can only be specified on an interface " + candidateComponent.getBeanClassName());

                Map<String, Object> attributes = annotationMetadata
                                                     .getAnnotationAttributes(ShenyuClient.class.getCanonicalName());

                String name = getClientName(attributes);
                LOG.info("configuration, attributes:{}", JsonUtils.toJson(attributes));
//                registerClientConfiguration(registry, name, attributes.get("configuration"));
                registerShenyuClient(registry, annotationMetadata, attributes);
            }
        }
    }

    private void registerShenyuClient(final BeanDefinitionRegistry registry, final AnnotationMetadata annotationMetadata, final Map<String, Object> attributes) {
        String className = annotationMetadata.getClassName();
        ConfigurableBeanFactory beanFactory = registry instanceof ConfigurableBeanFactory ? (ConfigurableBeanFactory) registry : null;

        String name = getName(attributes);
        String contextId = getContextId(beanFactory, attributes);

        Class clazz = ClassUtils.resolveClassName(className, null);
        FeignClientFactoryBean factoryBean = new FeignClientFactoryBean();
        factoryBean.setBeanFactory(beanFactory);
        factoryBean.setName(name);
        factoryBean.setContextId(contextId);
        factoryBean.setType(clazz);
        factoryBean.setRefreshableClient(isClientRefreshEnabled());
        BeanDefinitionBuilder definition = BeanDefinitionBuilder.genericBeanDefinition(clazz, () -> {
            factoryBean.setUrl(name);
            factoryBean.setPath(getPath(beanFactory, attributes));
            factoryBean.setDismiss404(Boolean.parseBoolean(String.valueOf(attributes.get("dismiss404"))));
            Object fallback = attributes.get("fallback");
            if (Objects.nonNull(fallback)) {
                factoryBean.setFallback(fallback instanceof Class ? (Class<?>) fallback
                                            : ClassUtils.resolveClassName(fallback.toString(), null));
            }
            Object fallbackFactory = attributes.get("fallbackFactory");
            if (Objects.nonNull(fallbackFactory)) {
                factoryBean.setFallbackFactory(fallbackFactory instanceof Class ? (Class<?>) fallbackFactory
                                                   : ClassUtils.resolveClassName(fallbackFactory.toString(), null));
            }
            factoryBean.addCustomizer(builder -> {
                BeanFactory factory = beanFactory;
                if (Objects.isNull(beanFactory)) {
                    factory = factoryBean.getApplicationContext();
                }
                ShenyuClientCapability.INSTANCE.setBeanFactory(factory);
                builder.addCapability(ShenyuClientCapability.INSTANCE);
            });
            return factoryBean.getObject();
        });
        definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
        definition.setLazyInit(true);
        validate(attributes);

        AbstractBeanDefinition beanDefinition = definition.getBeanDefinition();
        beanDefinition.setAttribute(FactoryBean.OBJECT_TYPE_ATTRIBUTE, className);
        beanDefinition.setAttribute("shenyuClientsRegistrarFactoryBean", factoryBean);

        // has a default, won't be null
        boolean primary = (Boolean) attributes.get("primary");

        beanDefinition.setPrimary(primary);

        String[] qualifiers = getQualifiers(attributes);
        if (ObjectUtils.isEmpty(qualifiers)) {
            qualifiers = new String[] {name + "ShenyuClient"};
        }
        BeanDefinitionHolder holder = new BeanDefinitionHolder(beanDefinition, className, qualifiers);
        BeanDefinitionReaderUtils.registerBeanDefinition(holder, registry);

    }

    private void validate(final Map<String, Object> attributes) {
        AnnotationAttributes annotation = AnnotationAttributes.fromMap(attributes);
        // This blows up if an aliased property is over specified
        validateFallback(annotation.getClass("fallback"));
        validateFallbackFactory(annotation.getClass("fallbackFactory"));
    }

    private String getContextId(final ConfigurableBeanFactory beanFactory, final Map<String, Object> attributes) {
        String contextId = (String) attributes.get("contextId");
        if (!StringUtils.hasText(contextId)) {
            return getName(attributes);
        }

        contextId = resolve(beanFactory, contextId);
        return getName(contextId);
    }

    private String resolve(final ConfigurableBeanFactory beanFactory, final String value) {
        if (StringUtils.hasText(value)) {
            if (Objects.isNull(beanFactory)) {
                return this.environment.resolvePlaceholders(value);
            }
            BeanExpressionResolver resolver = beanFactory.getBeanExpressionResolver();
            String resolved = beanFactory.resolveEmbeddedValue(value);
            if (Objects.isNull(resolver)) {
                return resolved;
            }
            Object evaluateValue = resolver.evaluate(resolved, new BeanExpressionContext(beanFactory, null));
            if (Objects.nonNull(evaluateValue)) {
                return String.valueOf(evaluateValue);
            }
            return null;
        }
        return value;
    }

    /**
     * Gets scanner.
     * @return the scanner
     */
    protected ClassPathScanningCandidateComponentProvider getScanner() {
        return new ClassPathScanningCandidateComponentProvider(false, this.environment) {
            @Override
            protected boolean isCandidateComponent(final AnnotatedBeanDefinition beanDefinition) {
                boolean isCandidate = false;
                if (beanDefinition.getMetadata().isIndependent()) {
                    if (!beanDefinition.getMetadata().isAnnotation()) {
                        isCandidate = true;
                    }
                }
                return isCandidate;
            }
        };
    }

    /**
     * Gets base packages.
     * @param importingClassMetadata the importing class metadata
     * @return the base packages
     */
    protected Set<String> getBasePackages(final AnnotationMetadata importingClassMetadata) {
        Map<String, Object> attributes = importingClassMetadata.getAnnotationAttributes(EnableShenyuClients.class.getCanonicalName());

        Set<String> basePackages = new HashSet<>();
        for (String pkg : (String[]) attributes.get("value")) {
            if (StringUtils.hasText(pkg)) {
                basePackages.add(pkg);
            }
        }
        for (String pkg : (String[]) attributes.get("basePackages")) {
            if (StringUtils.hasText(pkg)) {
                basePackages.add(pkg);
            }
        }
        for (Class<?> clazz : (Class[]) attributes.get("basePackageClasses")) {
            basePackages.add(ClassUtils.getPackageName(clazz));
        }

        if (basePackages.isEmpty()) {
            basePackages.add(ClassUtils.getPackageName(importingClassMetadata.getClassName()));
        }
        return basePackages;
    }

    private String getQualifier(final Map<String, Object> client) {
        if (Objects.isNull(client)) {
            return null;
        }
        String qualifier = (String) client.get("qualifier");
        if (StringUtils.hasText(qualifier)) {
            return qualifier;
        }
        return null;
    }

    private String[] getQualifiers(final Map<String, Object> client) {
        if (Objects.isNull(client)) {
            return null;
        }
        List<String> qualifierList = new ArrayList<>(Arrays.asList((String[]) client.get("qualifiers")));
        qualifierList.removeIf(qualifier -> !StringUtils.hasText(qualifier));
        if (qualifierList.isEmpty() && Objects.nonNull(getQualifier(client))) {
            qualifierList = Collections.singletonList(getQualifier(client));
        }
        return !qualifierList.isEmpty() ? qualifierList.toArray(new String[0]) : null;
    }

    @Override
    public void setEnvironment(final Environment environment) {
        this.environment = environment;
    }

    /**
     * Gets name.
     * @param name the name
     * @return the name
     */
    static String getName(final String name) {
        if (!StringUtils.hasText(name)) {
            return "";
        }
        String host;
        try {
            String url;
            if (!name.startsWith("http://") && !name.startsWith("https://")) {
                url = "http://" + name;
            } else {
                url = name;
            }
            host = new URI(url).getHost();

        } catch (URISyntaxException e) {
            host = null;
        }
        Assert.state(Objects.nonNull(host), "Service id not legal hostname (" + name + ")");
        return name;
    }

    /**
     * Gets name.
     * @param attributes the attributes
     * @return the name
     */
    String getName(final Map<String, Object> attributes) {
        return getName(null, attributes);
    }

    /**
     * Gets name.
     * @param beanFactory the bean factory
     * @param attributes  the attributes
     * @return the name
     */
    String getName(final ConfigurableBeanFactory beanFactory, final Map<String, Object> attributes) {
        String name = (String) attributes.get("name");
        if (!StringUtils.hasText(name)) {
            name = (String) attributes.get("value");
        }
        name = resolve(beanFactory, name);
        return getName(name);
    }

    /**
     * Gets url.
     * @param url the url
     * @return the url
     */
    static String getUrl(final String url) {
        String resultUrl = url;
        if (StringUtils.hasText(resultUrl) && !(resultUrl.startsWith("#{") && resultUrl.contains("}"))) {
            if (!resultUrl.contains("://")) {
                resultUrl = "http://" + resultUrl;
            }
            if (resultUrl.endsWith("/")) {
                resultUrl = resultUrl.substring(0, resultUrl.length() - 1);
            }
            try {
                new URL(resultUrl);
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException(resultUrl + " is malformed", e);
            }
        }
        return resultUrl;
    }

    private String getUrl(final ConfigurableBeanFactory beanFactory, final Map<String, Object> attributes) {
        String url = resolve(beanFactory, (String) attributes.get("url"));
        return getUrl(url);
    }

    private String getPath(final ConfigurableBeanFactory beanFactory, final Map<String, Object> attributes) {
        String path = resolve(beanFactory, (String) attributes.get("path"));
        return getPath(path);
    }

    /**
     * Gets path.
     * @param path the path
     * @return the path
     */
    static String getPath(final String path) {
        String resultPath = path;
        if (StringUtils.hasText(resultPath)) {
            resultPath = resultPath.trim();
            if (!resultPath.startsWith("/")) {
                resultPath = "/" + resultPath;
            }
            if (resultPath.endsWith("/")) {
                resultPath = resultPath.substring(0, resultPath.length() - 1);
            }
        }
        return resultPath;
    }

    private String getClientName(final Map<String, Object> client) {
        if (Objects.isNull(client)) {
            return null;
        }
        String value = (String) client.get("contextId");
        if (!StringUtils.hasText(value)) {
            value = (String) client.get("value");
        }
        if (!StringUtils.hasText(value)) {
            value = (String) client.get("name");
        }
        if (!StringUtils.hasText(value)) {
            value = (String) client.get("serviceId");
        }
        if (StringUtils.hasText(value)) {
            return value;
        }

        throw new IllegalStateException(
            "Either 'name' or 'value' must be provided in @" + FeignClient.class.getSimpleName());
    }

    private void registerClientConfiguration(final BeanDefinitionRegistry registry, final Object name, final Object configuration) {
        BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(FeignClientSpecification.class);
        builder.addConstructorArgValue(name);
        builder.addConstructorArgValue(configuration);
        registry.registerBeanDefinition(name + "." + FeignClientSpecification.class.getSimpleName(),
            builder.getBeanDefinition());
    }

    private boolean isClientRefreshEnabled() {
        return environment.getProperty("feign.client.refresh-enabled", Boolean.class, false);
    }

}

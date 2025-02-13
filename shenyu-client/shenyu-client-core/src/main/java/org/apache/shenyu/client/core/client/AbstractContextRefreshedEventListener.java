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

package org.apache.shenyu.client.core.client;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.utils.OpenApiUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.javatuples.Sextet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

/**
 * The type abstract context refreshed event listener.
 *
 * <p>See <a href="https://github.com/apache/shenyu/issues/415">upload dubbo metadata on ContextRefreshedEvent</a>
 */
public abstract class AbstractContextRefreshedEventListener<T, A extends Annotation> implements ApplicationListener<ContextRefreshedEvent> {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractContextRefreshedEventListener.class);

    /**
     * api path separator.
     */
    protected static final String PATH_SEPARATOR = "/";

    protected static final String EVERY_PATH = "**";

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private final Map<Method, MetaDataRegisterDTO> metaDataMap = new ConcurrentHashMap<>();

    private final String appName;

    private final String contextPath;

    private final String ipAndPort;

    private final String host;

    private final String port;

    private ApplicationContext context;

    private final Boolean isDiscoveryLocalMode;
    
    /**
     * multiple namespace support.
     */
    private final List<String> namespace;

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public AbstractContextRefreshedEventListener(final ShenyuClientConfig clientConfig,
                                                 final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        ClientPropertiesConfig config = clientConfig.getClient().get(getClientName());
        if (Objects.isNull(config)) {
            throw new ShenyuClientIllegalArgumentException("clientConfig must config " + getClientName() + " properties");
        }
        Properties props = config.getProps();
        String namespace = clientConfig.getNamespace();
        if (StringUtils.isBlank(namespace)) {
            LOG.warn("current shenyu.namespace is null, use default namespace: {}", Constants.SYS_DEFAULT_NAMESPACE_ID);
            namespace = Constants.SYS_DEFAULT_NAMESPACE_ID;
        }
        this.namespace = Lists.newArrayList(StringUtils.split(namespace, Constants.SEPARATOR_CHARS));
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.contextPath = Optional.ofNullable(props.getProperty(ShenyuClientConstants.CONTEXT_PATH)).map(UriUtils::repairData).orElse("");
        if (StringUtils.isBlank(appName) && StringUtils.isBlank(contextPath)) {
            String errorMsg = "client register param must config the appName or contextPath";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.ipAndPort = props.getProperty(ShenyuClientConstants.IP_PORT);
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        this.port = props.getProperty(ShenyuClientConstants.PORT);
        this.isDiscoveryLocalMode = Boolean.valueOf(props.getProperty(ShenyuClientConstants.DISCOVERY_LOCAL_MODE_KEY));
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(@NonNull final ContextRefreshedEvent event) {
        context = event.getApplicationContext();
        Map<String, T> beans = getBeans(context);
        if (MapUtils.isEmpty(beans)) {
            return;
        }
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (isDiscoveryLocalMode) {
            List<String> namespaceIds = this.getNamespace();
            namespaceIds.forEach(namespaceId -> publisher.publishEvent(buildURIRegisterDTO(context, beans, namespaceId)));
        }
        beans.forEach(this::handle);
        Map<String, Object> apiModules = context.getBeansWithAnnotation(ApiModule.class);
        apiModules.forEach((k, v) -> handleApiDoc(v, beans));
    }

    private void handleApiDoc(final Object bean, final Map<String, T> beans) {
        Class<?> apiModuleClass = AopUtils.isAopProxy(bean) ? AopUtils.getTargetClass(bean) : bean.getClass();
        ApiModule apiModule = apiModuleClass.getDeclaredAnnotation(ApiModule.class);
        if (Objects.nonNull(apiModule) && apiModule.generated()) {
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(apiModuleClass);
            for (Method method : methods) {
                if (method.isAnnotationPresent(ApiDoc.class)) {
                    List<ApiDocRegisterDTO> apis = buildApiDocDTO(bean, method, beans);
                    for (ApiDocRegisterDTO apiDocRegisterDTO : apis) {
                        publisher.publishEvent(apiDocRegisterDTO);
                    }
                }
            }
        }
    }

    private List<ApiDocRegisterDTO> buildApiDocDTO(final Object bean, final Method method, final Map<String, T> beans) {
        AtomicBoolean generated = new AtomicBoolean(false);
        Pair<String, List<String>> pairs = Stream.of(method.getDeclaredAnnotations()).filter(ApiDoc.class::isInstance).findAny().map(item -> {
            ApiDoc apiDoc = (ApiDoc) item;
            generated.set(apiDoc.generated());
            String[] tags = apiDoc.tags();
            List<String> tagsList = new ArrayList<>();
            if (tags.length > 0 && StringUtils.isNotBlank(tags[0])) {
                tagsList = Arrays.asList(tags);
            }
            return Pair.of(apiDoc.desc(), tagsList);
        }).orElse(Pair.of("", new ArrayList<>()));
        if (!generated.get()) {
            return Collections.emptyList();
        }
        Class<?> clazz = AopUtils.isAopProxy(bean) ? AopUtils.getTargetClass(bean) : bean.getClass();
        String superPath = buildApiSuperPath(clazz, AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType()));
        if (superPath.contains("*")) {
            superPath = superPath.substring(0, superPath.lastIndexOf("/"));
        }
        Annotation annotation = AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType());
        if (Objects.isNull(annotation)) {
            return Lists.newArrayList();
        }
        Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> sextet = buildApiDocSextet(method, annotation, beans);
        if (Objects.isNull(sextet)) {
            return Lists.newArrayList();
        }
        String contextPath = getContextPath();
        String[] value0 = sextet.getValue0();
        List<ApiDocRegisterDTO> list = Lists.newArrayList();
        for (String value : value0) {
            String apiPath = pathJoin(contextPath, superPath, value);
            ApiHttpMethodEnum[] value3 = sextet.getValue3();
            for (ApiHttpMethodEnum apiHttpMethodEnum : value3) {
                String documentJson = buildDocumentJson(pairs.getRight(), apiPath, method);
                String extJson = buildExtJson(method);
                ApiDocRegisterDTO build = ApiDocRegisterDTO.builder()
                        .consume(sextet.getValue1())
                        .produce(sextet.getValue2())
                        .httpMethod(apiHttpMethodEnum.getValue())
                        .contextPath(contextPath)
                        .ext(extJson)
                        .document(documentJson)
                        .rpcType(sextet.getValue4().getName())
                        .version(sextet.getValue5())
                        .apiDesc(pairs.getLeft())
                        .tags(pairs.getRight())
                        .apiPath(apiPath)
                        .apiSource(ApiSourceEnum.ANNOTATION_GENERATION.getValue())
                        .state(ApiStateEnum.UNPUBLISHED.getState())
                        .apiOwner("admin")
                        .eventType(EventType.REGISTER)
                        .build();
                list.add(build);
            }
        }
        return list;
    }

    private String buildExtJson(final Method method) {
        final MetaDataRegisterDTO metaData = metaDataMap.get(method);
        if (Objects.isNull(metaData)) {
            return "{}";
        }
        ApiDocRegisterDTO.ApiExt ext = new ApiDocRegisterDTO.ApiExt();
        ext.setHost(getHost());
        ext.setPort(Integer.valueOf(getPort()));
        ext.setServiceName(metaData.getServiceName());
        ext.setMethodName(metaData.getMethodName());
        ext.setParameterTypes(metaData.getParameterTypes());
        ext.setRpcExt(metaData.getRpcExt());
        ext = customApiDocExt(ext);
        return GsonUtils.getInstance().toJson(ext);
    }

    protected ApiDocRegisterDTO.ApiExt customApiDocExt(final ApiDocRegisterDTO.ApiExt ext) {
        return ext;
    }

    private String buildDocumentJson(final List<String> tags, final String path, final Method method) {
        Map<String, Object> documentMap = ImmutableMap.<String, Object>builder()
                .put("tags", tags)
                .put("operationId", path)
                .put("parameters", OpenApiUtils.generateDocumentParameters(path, method))
                .put("responses", OpenApiUtils.generateDocumentResponse(path)).build();
        return GsonUtils.getInstance().toJson(documentMap);
    }

    protected abstract Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(Method method, Annotation annotation, Map<String, T> beans);

    protected abstract Map<String, T> getBeans(ApplicationContext context);

    @SuppressWarnings("all")
    protected abstract URIRegisterDTO buildURIRegisterDTO(ApplicationContext context,
                                                          Map<String, T> beans,
                                                          String namespaceId);
    
    protected abstract String getClientName();

    protected void handle(final String beanName, final T bean) {
        Class<?> clazz = getCorrectedClass(bean);
        final A beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType());
        final String superPath = buildApiSuperPath(clazz, beanShenyuClient);
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            handleClass(clazz, bean, beanShenyuClient, superPath);
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            handleMethod(bean, clazz, beanShenyuClient, method, superPath);
        }
    }

    protected Class<?> getCorrectedClass(final T bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        return clazz;
    }

    protected abstract String buildApiSuperPath(Class<?> clazz,
                                                @Nullable A beanShenyuClient);

    protected void handleClass(final Class<?> clazz,
                               final T bean,
                               @NonNull final A beanShenyuClient,
                               final String superPath) {
        List<String> namespaceIds = this.getNamespace();
        for (String namespaceId : namespaceIds) {
            final MetaDataRegisterDTO metaData = buildMetaDataDTO(bean, beanShenyuClient,
                    pathJoin(contextPath, superPath), clazz, null, namespaceId);
            publisher.publishEvent(metaData);
        }
    }

    protected void handleMethod(final T bean,
                                final Class<?> clazz,
                                @Nullable final A beanShenyuClient,
                                final Method method,
                                final String superPath) {
        A methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, getAnnotationType());
        if (Objects.nonNull(methodShenyuClient)) {
            List<String> namespaceIds = this.getNamespace();
            for (String namespaceId : namespaceIds) {
                final MetaDataRegisterDTO metaData = buildMetaDataDTO(bean, methodShenyuClient,
                        buildApiPath(method, superPath, methodShenyuClient), clazz, method, namespaceId);
                publisher.publishEvent(metaData);
                metaDataMap.put(method, metaData);
            }
        }
    }

    protected abstract Class<A> getAnnotationType();

    protected abstract String buildApiPath(Method method,
                                           String superPath,
                                           @NonNull A methodShenyuClient);

    protected String pathJoin(@NonNull final String... path) {
        StringBuilder result = new StringBuilder(PATH_SEPARATOR);
        for (String p : path) {
            if (!result.toString().endsWith(PATH_SEPARATOR)) {
                result.append(PATH_SEPARATOR);
            }
            result.append(p.startsWith(PATH_SEPARATOR) ? p.replaceFirst(PATH_SEPARATOR, "") : p);
        }
        return result.toString();
    }
    
    protected List<String> getNamespace() {
        return namespace;
    }

    protected abstract MetaDataRegisterDTO buildMetaDataDTO(T bean,
                                                            @NonNull A shenyuClient,
                                                            String path,
                                                            Class<?> clazz,
                                                            Method method,
                                                            String namespaceId);

    /**
     * Get the event publisher.
     *
     * @return the event publisher
     */
    public ShenyuClientRegisterEventPublisher getPublisher() {
        return publisher;
    }

    /**
     * Get the metadata map.
     *
     * @return the metadata map
     */
    public Map<Method, MetaDataRegisterDTO> getMetaDataMap() {
        return metaDataMap;
    }

    /**
     * Get the app name.
     *
     * @return the app name
     */
    public String getAppName() {
        return appName;
    }

    /**
     * Get the context path.
     *
     * @return the context path
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * Get the ip and port.
     *
     * @return the ip and port
     */
    public String getIpAndPort() {
        return ipAndPort;
    }

    /**
     * Get the host.
     *
     * @return the host
     */
    public String getHost() {
        return IpUtils.isCompleteHost(this.host) ? this.host
                : IpUtils.getHost(this.host);
    }

    /**
     * Get the port.
     *
     * @return the port
     */
    public String getPort() {
        return StringUtils.isBlank(this.port) ? "-1" : this.port;
    }

    /**
     * Get the context.
     *
     * @return the context
     */
    public ApplicationContext getContext() {
        return context;
    }
}

package org.apache.shenyu.client.core.client;

import com.google.common.collect.Lists;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.javatuples.Sextet;
import org.springframework.aop.support.AopUtils;
import org.springframework.core.annotation.AnnotatedElementUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author zhengpeng
 * @date 2022/12/28 4:07 下午
 **/
public abstract class AbstractApiDocEventListener<T, A extends Annotation> extends AbstractContextRefreshedEventListener{

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public AbstractApiDocEventListener(PropertiesConfig clientConfig, ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected List<ApiDocRegisterDTO> buildApiDocDTO(Object bean, Method method) {
        String apiDesc = Stream.of(method.getDeclaredAnnotations()).filter(item -> item instanceof ApiDoc).findAny().map(item -> {
            ApiDoc apiDoc = (ApiDoc) item;
            return apiDoc.desc();
        }).orElse("");
        Class<?> clazz = AopUtils.isAopProxy(bean) ? AopUtils.getTargetClass(bean) : bean.getClass();
        String superPath = buildApiSuperPath(clazz, AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType()));
        if (superPath.indexOf("*") > 0) {
            superPath = superPath.substring(0, superPath.lastIndexOf("/"));
        }
        Annotation annotation = AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType());
        if(Objects.isNull(annotation)){
            return Lists.newArrayList();
        }
        Sextet<String[],String,String, ApiHttpMethodEnum[], RpcTypeEnum,String> sextet = buildApiDocSextet(method,annotation);
        if(Objects.isNull(sextet)){
            return Lists.newArrayList();
        }
        String contextPath = getContextPath();
        String[] value0 = sextet.getValue0();
        List<ApiDocRegisterDTO> list = Lists.newArrayList();
        for (String value : value0) {
            String apiPath = contextPath + superPath + value;
            ApiHttpMethodEnum[] value3 = sextet.getValue3();
            for (ApiHttpMethodEnum apiHttpMethodEnum : value3) {
                ApiDocRegisterDTO build = ApiDocRegisterDTO.builder()
                        .consume(sextet.getValue1())
                        .produce(sextet.getValue2())
                        .httpMethod(apiHttpMethodEnum.getValue())
                        .contextPath(contextPath)
                        .ext("{}")
                        .document("{}")
                        .rpcType(sextet.getValue4().getName())
                        .version(sextet.getValue5())
                        .apiDesc(apiDesc)
                        .apiPath(apiPath)
                        .apiSource(ApiSourceEnum.ANNOTATION_GENERATION.getValue())
                        .state(ApiStateEnum.PUBLISHED.getState())
                        .apiOwner("admin")
                        .eventType(EventType.REGISTER)
                        .build();
                list.add(build);
            }
        }
        return list;
    }

    /**
     *
     * @return
     */
    protected abstract Sextet<String[],String,String,ApiHttpMethodEnum[],RpcTypeEnum,String> buildApiDocSextet(Method method,Annotation annotation);
}

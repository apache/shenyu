package org.dromara.soul.web.plugin.dubbo;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Splitter;

/**
 * multiple generic param resolve service
 * 
 * @author feixue
 * @date 2020-04-03 10:07
 */
public class MultipleGenericParamResolveServiceImpl implements GenericParamResolveService {

    private static final Logger LOG = LoggerFactory.getLogger(MultipleGenericParamResolveServiceImpl.class);

    @Override
    public Pair<String[], Object[]> buildParameter(String body, String parameterTypes) {
        Map<String, Object> paramMap;

        // when dubbo register service, multi parameters separated by a comma
        List<String> parameterTypeList = Splitter.on(",").splitToList(parameterTypes);

        // init param value
        Object[] dests = new Object[parameterTypeList.size()];
        try {
            // avoid restart sorting after json converted map by jackson, so i use LinkedHashMap
            paramMap = new ObjectMapper().readValue(body, LinkedHashMap.class);

            // Traverse dubbo param
            int index = 0;
            for (Map.Entry<String, Object> paramEntry : paramMap.entrySet()) {
                try {
                    String parameterType = parameterTypeList.get(index);
                    Class<?> c = Class.forName(parameterType);
                    if (c.isArray()) {
                        c = c.getComponentType();
                    }

                    //current parameter type is Collection or Map
                    boolean isCollectionOrMap = false;
                    if(Collection.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c)) {
                        isCollectionOrMap = true;
                    }

                    // current parameter type is basic type or basic type arrays
                    boolean isPrimitiveOrWrapped = ClassUtils.isPrimitiveOrWrapper(c);
                    if (isPrimitiveOrWrapped || c.equals(String.class) || isCollectionOrMap) {
                        dests[index] = paramEntry.getValue();
                    }

                } catch (ClassNotFoundException e) {
                    // dubbo parameter type is custom javabean object
                    LOG.error("current parameter type is custom javabean object, message={}", e.getMessage());
                    dests[index] = paramEntry.getValue();
                }

                index++;
            }

        } catch (Exception e) {
            LOG.error("multi parameter build fail, message={}", e.getMessage());
        }

        // Pair right 根据参数个数创建对应个数的Object
        return new ImmutablePair<>(parameterTypeList.toArray(new String[parameterTypeList.size()]), dests);
    }
}

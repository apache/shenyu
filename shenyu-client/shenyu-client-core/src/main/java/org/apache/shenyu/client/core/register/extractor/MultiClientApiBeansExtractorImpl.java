package org.apache.shenyu.client.core.register.extractor;

import org.apache.shenyu.client.core.register.ApiBean;
import org.springframework.context.ApplicationContext;

import java.util.List;
import java.util.stream.Collectors;

/**
 * MultiClientApiBeansExtractorImpl.
 * Multi-client collector
 */
public class MultiClientApiBeansExtractorImpl implements ApiBeansExtractor {
    
    private final List<RpcApiBeansExtractor> rpcApiBeansExtractors;
    
    public MultiClientApiBeansExtractorImpl(final List<RpcApiBeansExtractor> rpcApiBeansExtractors) {
        this.rpcApiBeansExtractors = rpcApiBeansExtractors;
    }
    
    @Override
    public List<ApiBean> extract(final ApplicationContext applicationContext) {
        return rpcApiBeansExtractors.stream()
                .flatMap(e -> e.extract(applicationContext).stream())
                .collect(Collectors.toList());
    }
}

package org.apache.shenyu.k8s;

import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.models.*;
import org.apache.shenyu.common.config.ssl.ShenyuSniAsyncMapping;
import org.apache.shenyu.k8s.parser.IngressParser;
import org.apache.shenyu.k8s.reconciler.IngressReconciler;
import org.apache.shenyu.k8s.repository.ShenyuCacheRepository;
import org.junit.Before;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

final public class IngressReconcilerTest {

    private SharedIndexInformer<V1Ingress> ingressInformer;

    private SharedIndexInformer<V1Secret> secretInformer;

    private ShenyuCacheRepository shenyuCacheRepository;

    private ShenyuSniAsyncMapping shenyuSniAsyncMapping;

    private SharedIndexInformer<V1Service> serviceInformer;

    private SharedIndexInformer<V1Endpoints> endpointsInformer;

    private IngressReconciler ingressReconciler;

    @BeforeEach
    public void init() {
        ingressInformer = mock(SharedIndexInformer.class);
        secretInformer = mock(SharedIndexInformer.class);
        shenyuCacheRepository = mock(ShenyuCacheRepository.class);
        shenyuSniAsyncMapping = new ShenyuSniAsyncMapping();
        serviceInformer = mock(SharedIndexInformer.class);
        endpointsInformer = mock(SharedIndexInformer.class);

        // mock ingressInformer
        Indexer<V1Ingress> ingressIndexer = mock(Indexer.class);
        V1IngressRule mockedRule = new V1IngressRuleBuilder().withNewHttp().withPaths(
                        new V1HTTPIngressPathBuilder().withPath("/**").withNewBackend().withNewService().withName("testService").withNewPort().withNumber(8189).endPort().endService().endBackend().build())
                .endHttp().build();
        Map<String, String> annotations = new HashMap<>();
        annotations.put("kubernetes.io/ingress.class", "shenyu");
        V1Ingress mockedIngress = new V1IngressBuilder().withNewMetadata().withName("mockedIngress").withNamespace("mockedNamespace").withAnnotations(annotations).endMetadata()
                .withNewSpec().withRules(mockedRule).endSpec()
                .withKind("Ingress").build();
        when(ingressIndexer.getByKey("mockedNamespace/mockedIngress")).thenReturn(mockedIngress);
        when(ingressInformer.getIndexer()).thenReturn(ingressIndexer);

        // mock endpointsLister
        Indexer<V1Endpoints> endpointsIndexer = mock(Indexer.class);
        V1Endpoints mockedEndpoints = new V1EndpointsBuilder().withKind("Endpoints")
                .withNewMetadata().withNamespace("mockedNamespace").withName("testService").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build())
                .build();
        when(endpointsIndexer.getByKey("mockedNamespace/testService")).thenReturn(mockedEndpoints);
        when(endpointsInformer.getIndexer()).thenReturn(endpointsIndexer);

        IngressParser ingressParser = new IngressParser(serviceInformer, endpointsInformer);
        ApiClient apiClient = mock(ApiClient.class);

        ingressReconciler = new IngressReconciler(ingressInformer, secretInformer, shenyuCacheRepository,
                shenyuSniAsyncMapping, ingressParser, apiClient);
    }

    @Test
    public void testReconcile() {
        Result result = ingressReconciler.reconcile(new Request("mockedNamespace", "mockedIngress"));
        Assertions.assertEquals(new Result(false), result);
    }
}

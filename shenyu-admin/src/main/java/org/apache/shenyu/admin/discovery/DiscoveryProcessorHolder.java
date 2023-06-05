package org.apache.shenyu.admin.discovery;

public class DiscoveryProcessorHolder {

    private final DiscoveryProcessor defaultDiscoveryProcessor;

    private final DiscoveryProcessor localDiscoveryProcessor;

    public DiscoveryProcessorHolder(final DiscoveryProcessor defaultDiscoveryProcessor, final DiscoveryProcessor localDiscoveryProcessor) {
        this.defaultDiscoveryProcessor = defaultDiscoveryProcessor;
        this.localDiscoveryProcessor = localDiscoveryProcessor;
    }

    public DiscoveryProcessor chooseProcessor(String mode) {
        if (Mode.LOCAL.name().equalsIgnoreCase(mode)) {
            return localDiscoveryProcessor;
        } else {
            return defaultDiscoveryProcessor;
        }
    }

    enum Mode {
        LOCAL,
        ZOOKEEPER,
        NACOS,
        EUREKA
    }


}

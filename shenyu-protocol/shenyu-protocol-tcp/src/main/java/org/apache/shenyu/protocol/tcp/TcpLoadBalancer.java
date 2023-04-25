//package org.apache.shenyu.protocol.tcp;
//
//
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import reactor.core.Disposable;
//import reactor.core.publisher.Mono;
//import reactor.netty.Connection;
//import reactor.netty.tcp.TcpServer;
//
//import java.net.SocketAddress;
//import java.util.*;
//import java.util.concurrent.ConcurrentHashMap;
//
//
//public class TcpLoadBalancer {
//    final Logger LOGGER = LoggerFactory.getLogger(TcpLoadBalancer.class);
//    private volatile boolean stopped = false;
//    private List<String> targetHosts;
//    private int targetPort;
//    //private ConnectionProvider provider;
//    private Map<String, Disposable> disposables = new ConcurrentHashMap<>();
//    private TcpServer server;
//    private Disposable subscription;
//
//    public void start(List<String> targetHosts, int targetPort, LoadBalancerStrategy strategy, Duration connectTimeout) {
//        Objects.requireNonNull(targetHosts, "Target hosts list must not be null");
//        this.targetHosts = Collections.unmodifiableList(new ArrayList<>(targetHosts));
//        this.targetPort = targetPort;
//        provider = ConnectionProvider.fixed(strategy.getNumberOfConnections(), connectTimeout);
//        provider.connectNow();
//        for (String targetHost : targetHosts) {
//            Connection conn = TcpClient.create()
//                    .host(targetHost)
//                    .port(targetPort)
//                    .connectNow();
//            disposables.put(targetHost, conn.onDispose().subscribe(() -> {
//                LOGGER.info("Connection to {}:{} closed", conn.address().getHostString(), conn.address().getPort());
//                provider.dispose(conn);
//                Connection newConn = TcpClient.create()
//                        .host(targetHost)
//                        .port(targetPort)
//                        .connectNow();
//                disposables.put(targetHost, newConn.onDispose().subscribe(() -> {
//                    LOGGER.info("Connection to {}:{} closed", newConn.address().getHostString(), newConn.address().getPort());
//                    provider.dispose(newConn);
//                }));
//            }));
//            provider.add(conn);
//        }
//        subscription = TcpServer.create()
//                .handle((in, out) -> {
//                    Mono.fromCallable(() -> provider.select())
//                            .flatMap(conn -> {
//                                LOGGER.debug("Selected connection to {}:{}", conn.address().getHostString(), conn.address().getPort());
//                                return conn.outbound()
//                                        .send(in.receive().retain())
//                                        .then(
//                                                conn.inbound()
//                                                        .receive()
//                                                        .take(1)
//                                                        .doFinally(sig -> provider.release(conn))
//                                                        .subscribeOn(Schedulers.parallel())
//                                        );
//                            })
//                            .subscribe(out::send);
//                })
//                .bindNow();
//        LOGGER.info("TcpLoadBalancer started with {} connections to {}", targetHosts.size(), targetHosts);
//    }
//
//    public synchronized void updateTargetHosts(List<String> targetHosts) {
//        Objects.requireNonNull(targetHosts, "Target hosts list must not be null");
//        this.targetHosts = Collections.unmodifiableList(new ArrayList<>(targetHosts));
//        List<Mono<? extends Connection>> connections = new ArrayList<>();
//        for (String targetHost : targetHosts) {
//            if (!disposables.containsKey(targetHost)) {
//                Connection conn = TcpClient.create()
//                        .host(targetHost)
//                        .port(targetPort)
//                        .connectNow();
//                disposables.put(targetHost, conn.onDispose().subscribe(() -> {
//                    LOGGER.info("Connection to {}:{} closed", conn.address().getHostString(), conn.address().getPort());
//                    provider.dispose(conn);
//                    Connection newConn = TcpClient.create()
//                            .host(targetHost)
//                            .port(targetPort)
//                            .connectNow();
//                    disposables.put(targetHost, newConn.onDispose().subscribe(() -> {
//                        LOGGER.info("Connection to {}:{} closed", newConn.address().getHostString(), newConn.address().getPort());
//                        provider.dispose(newConn);
//                    }));
//                }));
//                connections.add(provider.acquire(Mono.just(conn)));
//            }
//        }
//        provider.updateConnections(connections);
//        LOGGER.info("TcpLoadBalancer updated with {} connections to {}", targetHosts.size(), targetHosts);
//    }
//
//    public void stop() {
//        stopped = true;
//        if (subscription != null) {
//            subscription.dispose();
//        }
//        for (Disposable disposable : disposables.values()) {
//            disposable.dispose();
//        }
//        if (provider != null) {
//            provider.disposeLater()
//                    .onDispose()
//                    .block();
//        }
//        LOGGER.info("TcpLoadBalancer stopped");
//    }
//
//    public static void main(String[] args) throws InterruptedException {
//        TcpLoadBalancer balancer = new TcpLoadBalanceradBalancer();
//        balancer.start(Arrays.asList("localhost", "127.0.0.1"), 8000, LoadBalancerStrategy.ROUND_ROBIN, Duration.ofSeconds(10));
//        // 模拟动态更新
//        Thread.sleep(5000);
//        balancer.updateTargetHosts(Arrays.asList("localhost", "127.0.0.2"));
//        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
//            balancer.stop();
//        }));
//    }
//}
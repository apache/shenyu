package org.apache.shenyu.protocol.tcp;


public class ShenyuTcpConfig {

    private Integer port;

    private Integer bossThreads;


    private Integer workerThreads;


    private Boolean keepLived;


    public Integer getPort() {
        return port;
    }

    public void setPort(Integer port) {
        this.port = port;
    }

    public Integer getBossThreads() {
        return bossThreads;
    }

    public void setBossThreads(Integer bossThreads) {
        this.bossThreads = bossThreads;
    }

    public Integer getWorkerThreads() {
        return workerThreads;
    }

    public void setWorkerThreads(Integer workerThreads) {
        this.workerThreads = workerThreads;
    }

    public Boolean getKeepLived() {
        return keepLived;
    }

    public void setKeepLived(Boolean keepLived) {
        this.keepLived = keepLived;
    }
}

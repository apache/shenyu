package org.apache.shenyu.springboot.starter.netty.config;

/**
 * Netty socket channel config.
 */
public class SocketChannelConfig extends ChannelConfig {

    private boolean soKeepalive;

    private boolean soReuseaddr;

    private int soLinger = -1;

    private boolean tcpNodelay = true;

    private int soRcvbuf = 87380;

    private int soSndbuf = 16384;

    private int ipTos;

    private boolean allowHalfClosure;

    /**
     * get soKeepalive.
     *
     * @return soKeepalive
     */
    public boolean isSoKeepalive() {
        return soKeepalive;
    }

    /**
     * get SoReuseaddr.
     *
     * @return soReuseaddr
     */
    public boolean isSoReuseaddr() {
        return soReuseaddr;
    }

    /**
     * get soLinger.
     *
     * @return soLinger
     */
    public int getSoLinger() {
        return soLinger;
    }

    /**
     * get tcpNodelay.
     *
     * @return tcpNodelay
     */
    public boolean isTcpNodelay() {
        return tcpNodelay;
    }

    /**
     * get soRcvbuf.
     *
     * @return soRcvbuf
     */
    public int getSoRcvbuf() {
        return soRcvbuf;
    }

    /**
     * get soSndbuf.
     *
     * @return soSndbuf
     */
    public int getSoSndbuf() {
        return soSndbuf;
    }

    /**
     * get ipTos.
     * @return ipTos
     */
    public int getIpTos() {
        return ipTos;
    }

    /**
     * get isAllowHalfClosure.
     *
     * @return isAllowHalfClosure
     */
    public boolean isAllowHalfClosure() {
        return allowHalfClosure;
    }

    /**
     * set soKeepalive.
     *
     * @param soKeepalive SO_KEEPALIVE
     */
    public void setSoKeepalive(final boolean soKeepalive) {
        this.soKeepalive = soKeepalive;
    }

    /**
     * ser setSoReuseaddr.
     *
     * @param soReuseaddr SO_REUSEADDR
     */
    public void setSoReuseaddr(final boolean soReuseaddr) {
        this.soReuseaddr = soReuseaddr;
    }

    /**
     * set soLinger.
     *
     * @param soLinger SO_LINGER
     */
    public void setSoLinger(final int soLinger) {
        this.soLinger = soLinger;
    }

    /**
     * set tcpNodelay.
     *
     * @param tcpNodelay TCP_NODELAY
     */
    public void setTcpNodelay(final boolean tcpNodelay) {
        this.tcpNodelay = tcpNodelay;
    }

    /**
     * set soRcvbuf.
     *
     * @param soRcvbuf SO_RCVBUF
     */
    public void setSoRcvbuf(final int soRcvbuf) {
        this.soRcvbuf = soRcvbuf;
    }

    /**
     * set soSndbuf.
     *
     * @param soSndbuf SO_SNDBUF
     */
    public void setSoSndbuf(final int soSndbuf) {
        this.soSndbuf = soSndbuf;
    }

    /**
     * set ipTos.
     *
     * @param ipTos IP_TOS
     */
    public void setIpTos(final int ipTos) {
        this.ipTos = ipTos;
    }

    /**
     * set allowHalfClosure.
     *
     * @param allowHalfClosure ALLOW_HALF_CLOSURE
     */
    public void setAllowHalfClosure(final boolean allowHalfClosure) {
        this.allowHalfClosure = allowHalfClosure;
    }
}

package org.apache.shenyu.springboot.starter.netty.config;

/**
 * Netty server socket channel config.
 */
public class ServerSocketChannelConfig extends ChannelConfig {

    private int soRcvbuf = 87380;

    private int soBacklog = 128;

    private boolean soReuseaddr;


    /**
     * get soRcvbuf.
     *
     * @return soRcvbuf
     */
    public int getSoRcvbuf() {
        return soRcvbuf;
    }

    /**
     * get soBacklog.
     *
     * @return soBacklog
     */
    public int getSoBacklog() {
        return soBacklog;
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
     * set soRcvbuf.
     *
     * @param soRcvbuf SO_RCVBUF
     */
    public void setSoRcvbuf(final int soRcvbuf) {
        this.soRcvbuf = soRcvbuf;
    }

    /**
     * set soBacklog.
     *
     * @param soBacklog SO_BACKLOG
     */
    public void setSoBacklog(final int soBacklog) {
        this.soBacklog = soBacklog;
    }

    /**
     * ser setSoReuseaddr.
     *
     * @param soReuseaddr SO_REUSEADDR
     */
    public void setSoReuseaddr(final boolean soReuseaddr) {
        this.soReuseaddr = soReuseaddr;
    }

}

package org.dromara.soul.common.utils;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.regex.Pattern;

/**
 * The type Ur i utils.
 *
 * @author xiaoyu(Myth)
 */
public class UrlUtils {

    private static final Pattern PATTERN = Pattern
            .compile("(http:\\/\\/|https:\\/\\/)?(?:(?:[0,1]?\\d?\\d|2[0-4]\\d|25[0-5])\\.){3}(?:[0,1]?\\d?\\d|2[0-4]\\d|25[0-5]):\\d{0,5}");

    private static final String HTTP = "http";

    /**
     * Check url boolean.
     *
     * @param url the url
     * @return the boolean
     */
    public static boolean checkUrl(final String url) {
        if (StringUtils.isBlank(url)) {
            return false;
        }
        if (checkIP(url)) {
            String[] hostPort;
            if (url.startsWith(HTTP)) {
                final String[] http = StringUtils.split(url, "\\/\\/");
                hostPort = StringUtils.split(http[1], Constants.COLONS);
            } else {
                hostPort = StringUtils.split(url, Constants.COLONS);
            }
            return isHostConnector(hostPort[0], Integer.parseInt(hostPort[1]));
        } else {
            return isHostReachable(url);
        }
    }

    private static boolean checkIP(final String url) {
        return PATTERN.matcher(url).matches();
    }

    private static boolean isHostConnector(final String host, final int port) {
        Socket socket = new Socket();
        try {
            socket.connect(new InetSocketAddress(host, port));
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        } finally {
            try {
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return true;
    }

    private static boolean isHostReachable(final String host) {
        try {
            return InetAddress.getByName(host).isReachable(1000);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    public static void main(String[] args) {
        final boolean success = checkUrl("www.baidu.com");
        System.out.println(success);
    }

}

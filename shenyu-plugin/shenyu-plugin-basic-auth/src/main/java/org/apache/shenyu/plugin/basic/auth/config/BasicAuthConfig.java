package org.apache.shenyu.plugin.basic.auth.config;

import java.io.Serializable;

/**
 * @author romic
 * @date 2022/7/19
 */
public class BasicAuthConfig implements Serializable {
    /**
     * private key.
     */
    private String username;

    /**
     * password
     */
    private String password;

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}

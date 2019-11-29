package org.dromara.soul.test.springcloud.dto;

import java.io.Serializable;

/**
 * The type User dto.
 *
 * @author xiaoyu(Myth)
 */
public class UserDTO implements Serializable {

    private String userId;

    /**
     * Gets user id.
     *
     * @return the user id
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Sets user id.
     *
     * @param userId the user id
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    @Override
    public String toString() {
        return "UserDTO{" +
                "userId='" + userId + '\'' +
                '}';
    }
}

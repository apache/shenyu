package org.dromara.soul.admin.query;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * this is permission query.
 *
 * @author nuo-promise
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PermissionQuery implements Serializable {

    /**
     * object id : role id or user id.
     */
    private String objectId;

    /**
     * resource id.
     */
    private String resourceId;
}

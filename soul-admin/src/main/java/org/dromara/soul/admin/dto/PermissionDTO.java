package org.dromara.soul.admin.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * this is permission Dto.
 *
 * @author nuo-promise
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PermissionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user key or role key.
     */
    private String objectId;

    /**
     * resource key.
     */
    private String resourceId;
}

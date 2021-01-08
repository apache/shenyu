package org.dromara.soul.admin.query;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.dromara.soul.admin.page.PageParameter;

import java.io.Serializable;

/**
 * this is resource query.
 *
 * @author nuo-promise
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ResourceQuery implements Serializable {

    /**
     * resource title.
     */
    private String title;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;
}

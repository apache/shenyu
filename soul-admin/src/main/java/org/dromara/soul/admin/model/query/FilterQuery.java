package org.dromara.soul.admin.model.query;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * data permission filter query.
 *
 * @author nuo-promise
 */
@Data
public class FilterQuery implements Serializable {

    private static final long serialVersionUID = 9107238465094879060L;

    /**
     * filter ids.
     */
    private List<String> filterIds;
}

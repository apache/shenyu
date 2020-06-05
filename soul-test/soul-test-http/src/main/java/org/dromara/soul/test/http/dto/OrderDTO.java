package org.dromara.soul.test.http.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * The type Order dto.
 *
 * @author xiaoyu(Myth)
 */
@Data
public class OrderDTO implements Serializable {

    private String id;

    private String name;

}

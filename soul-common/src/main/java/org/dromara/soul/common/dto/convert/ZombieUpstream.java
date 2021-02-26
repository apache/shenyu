package org.dromara.soul.common.dto.convert;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * this is zombie divide upstream.
 * @author zhangzm
 * @date 2021/2/25 19:08
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class ZombieUpstream {

    /**
     * divide upstream.
     */
    private DivideUpstream divideUpstream;

    /**
     * total check times.
     */
    private int zombieCheckTimes;

    /**
     * origin selector name.
     */
    private String selectorName;

    /**
     * create zombie upstream with divide upstream.
     * @param divideUpstream {@linkplain DivideUpstream} origin divide upstream.
     * @param zombieCheckTimes total check times.
     * @param selectorName origin selector name.
     * @return new zombie upstream.
     */
    public static ZombieUpstream transform(final DivideUpstream divideUpstream, final int zombieCheckTimes, final String selectorName) {
        return ZombieUpstream.builder().divideUpstream(divideUpstream).zombieCheckTimes(zombieCheckTimes).selectorName(selectorName).build();
    }
}

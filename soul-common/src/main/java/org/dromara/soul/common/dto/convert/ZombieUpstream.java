package org.dromara.soul.common.dto.convert;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
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

	private DivideUpstream divideUpstream;

	private int zombieCheckTimes;

	private String selectorName;

	public static ZombieUpstream transform(DivideUpstream divideUpstream,int zombieCheckTimes,String selectorName) {
		return ZombieUpstream.builder().divideUpstream(divideUpstream).zombieCheckTimes(zombieCheckTimes).selectorName(selectorName).build();
	}
}

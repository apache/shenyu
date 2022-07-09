/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.condition.judge;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.spi.Join;

/**
 * Contains(KMP algorithm) predicate judge.
 */
@Join
public class KMPContainsPredicateJudge implements PredicateJudge {

	@Override
	public Boolean judge(final ConditionData conditionData, final String realData) {
		return KMP.contains(realData, conditionData.getParamValue().trim());
	}

	/**
	 * KMP algorithm
	 */
	static class KMP {

		public static boolean contains(String source, CharSequence target) {
			int[] next = genNext(target);

			for (int i = 0, j = 0; i < source.length(); i++) {
				while (j > 0 && source.charAt(i) != target.charAt(j)) 
					j = next[j - 1];
				
				if(source.charAt(i) == target.charAt(j))
					j++;
				
				if(j == target.length())
					return true;
			}
			return false;
		}

		private static int[] genNext(CharSequence target) {
			int[] next = new int[target.length()];

			for (int i = 1, j = 0; i < target.length(); i++) {
				while (j > 0 && target.charAt(i) != target.charAt(j)) 
					j = next[j - 1];
				
				if (target.charAt(i) == target.charAt(j)) 
					j++;
				
				next[i] = j;
			}

			return next;
		}
	}
}
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

package org.apache.shenyu.loadbalancer.spi;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The load balancing device sends a probe request for each internal server，then decide select which server.
 */
@Join
public class ResponseTimeLoadBalancer extends AbstractLoadBalancer {

    private static final Logger LOG = LoggerFactory.getLogger(ResponseTimeLoadBalancer.class);

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        Map<String, Upstream> timecostMap = new ConcurrentHashMap<>();
        upstreamList.forEach(upstream -> timecostMap.put(computePingResponseTime(upstream.buildDomain()), upstream));
        List<String> timecostList = new ArrayList<>(timecostMap.keySet());
        Collections.sort(timecostList, String::compareTo);
        return timecostMap.get(timecostList.get(0));
    }

    /**
     * compute ping time.
     * @param ip server ip
     * @return response time
     */
    public static String computePingResponseTime(final String ip) {
        String time = "";
        String pingCmd = "ping " + ip;
        Runtime runtime = Runtime.getRuntime();

        try {
            Process process = runtime.exec(pingCmd);
            BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String inputLine = in.readLine();
            while (inputLine != null) {
                if (inputLine.contains("time")) {
                    time = inputLine.substring(inputLine.indexOf("time"));
                    break;
                }
                inputLine = in.readLine();
            }
            if (time.contains("=")) {
                time = time.substring(time.indexOf("=") + 1);
            }
        } catch (Exception ex) {
            LOG.error("computePingResponseTime error", ex);
        }
        return time;
    }

}

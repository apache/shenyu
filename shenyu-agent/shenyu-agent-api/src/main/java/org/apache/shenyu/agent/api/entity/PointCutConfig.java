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

package org.apache.shenyu.agent.api.entity;

import java.util.List;
import java.util.Map;

/**
 * The type Point cut config.
 */
public final class PointCutConfig {
    
    private List<PointCut> pointCuts;
    
    /**
     * Gets point cuts.
     *
     * @return the point cuts
     */
    public List<PointCut> getPointCuts() {
        return pointCuts;
    }
    
    /**
     * Sets point cuts.
     *
     * @param pointCuts the point cuts
     */
    public void setPointCuts(final List<PointCut> pointCuts) {
        this.pointCuts = pointCuts;
    }
    
    /**
     * The type Point cut.
     */
    public static final class PointCut {
        
        private String targetClass;
    
        private List<Point> points;
    
        private Map<String, List<String>> handlers;
    
        /**
         * Gets target class.
         *
         * @return the target class
         */
        public String getTargetClass() {
            return targetClass;
        }
    
        /**
         * Sets target class.
         *
         * @param targetClass the target class
         */
        public void setTargetClass(final String targetClass) {
            this.targetClass = targetClass;
        }
    
        /**
         * Gets points.
         *
         * @return the point cuts
         */
        public List<Point> getPoints() {
            return points;
        }
    
        /**
         * Sets points.
         *
         * @param points the points
         */
        public void setPoints(final List<Point> points) {
            this.points = points;
        }
    
        /**
         * Gets handlers.
         *
         * @return the handlers
         */
        public Map<String, List<String>> getHandlers() {
            return handlers;
        }
    
        /**
         * Sets handlers.
         *
         * @param handlers the handlers
         */
        public void setHandlers(final Map<String, List<String>> handlers) {
            this.handlers = handlers;
        }
    }
    
    /**
     * The type Point.
     */
    public static final class Point {
        
        private String type;
        
        private String name;
    
        /**
         * Gets type.
         *
         * @return the type
         */
        public String getType() {
            return type;
        }
    
        /**
         * Sets type.
         *
         * @param type the type
         */
        public void setType(final String type) {
            this.type = type;
        }
    
        /**
         * Gets name.
         *
         * @return the name
         */
        public String getName() {
            return name;
        }
    
        /**
         * Sets name.
         *
         * @param name the name
         */
        public void setName(final String name) {
            this.name = name;
        }
    }
}

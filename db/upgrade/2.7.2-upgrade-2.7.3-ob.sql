-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements.  See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership.  The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License.  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- this file works for OceanBase (MySQL mode).
ALTER TABLE `selector` RENAME COLUMN `loged` TO `logged`;
ALTER TABLE `rule` RENAME COLUMN `loged` TO `logged`;

-- Stop all Admin writers and back up instance_info before this one-time migration.
-- Retain the latest heartbeat, then latest update/create timestamp, then greatest id.
-- Rows upgraded from 2.7.1 have heartbeat 0 and are ordered by timestamps and id.
DELETE duplicate_instance FROM instance_info duplicate_instance
JOIN instance_info retained_instance ON
    retained_instance.namespace_id = duplicate_instance.namespace_id
    AND retained_instance.instance_ip = duplicate_instance.instance_ip
    AND retained_instance.instance_port = duplicate_instance.instance_port
    AND retained_instance.instance_type = duplicate_instance.instance_type
    AND (retained_instance.last_heart_beat_time > duplicate_instance.last_heart_beat_time
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated > duplicate_instance.date_updated)
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated = duplicate_instance.date_updated
            AND retained_instance.date_created > duplicate_instance.date_created)
        OR (retained_instance.last_heart_beat_time = duplicate_instance.last_heart_beat_time
            AND retained_instance.date_updated = duplicate_instance.date_updated
            AND retained_instance.date_created = duplicate_instance.date_created
            AND retained_instance.id > duplicate_instance.id));
CREATE UNIQUE INDEX uk_instance_info_identity ON instance_info (namespace_id, instance_ip, instance_port, instance_type);

-- Preserve complete API-debug request headers, including long Authorization values.
-- Stop Admin writers and back up mock_request_record before the one-time migration.
ALTER TABLE mock_request_record MODIFY COLUMN header TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'the request param in header';

-- Explicit, opt-in AI token reservations; existing rules remain disabled.
INSERT INTO `plugin_handle` (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('1960000000000007700', '51', 'reservationEnabled', 'reservationEnabled', 3, 2, 4, '{"required":"0","defaultValue":"false","rule":""}', '2026-10-19 00:00:00', '2026-10-19 00:00:00');
INSERT INTO `plugin_handle` (`id`, `plugin_id`, `field`, `label`, `data_type`, `type`, `sort`, `ext_obj`, `date_created`, `date_updated`) VALUES ('1960000000000007701', '51', 'reservationTokens', 'reservationTokens', 1, 2, 5, '{"required":"0","placeholder":"Required when reservation is enabled; estimated completion tokens per request","rule":""}', '2026-10-19 00:00:00', '2026-10-19 00:00:00');
INSERT INTO `shenyu_dict` (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1960000000000007702', 'reservationEnabled', 'RESERVATION_ENABLED', 'disabled', 'false', 'Enable estimated completion token reservations', 0, 1, '2026-10-19 00:00:00', '2026-10-19 00:00:00');
INSERT INTO `shenyu_dict` (`id`, `type`, `dict_code`, `dict_name`, `dict_value`, `desc`, `sort`, `enabled`, `date_created`, `date_updated`) VALUES ('1960000000000007703', 'reservationEnabled', 'RESERVATION_ENABLED', 'enabled', 'true', 'Enable estimated completion token reservations', 1, 1, '2026-10-19 00:00:00', '2026-10-19 00:00:00');

<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

# Client register provisioner

`ClientRegisterProvisioner` is an admin-side extension point for out-of-tree
plugins. It lets a plugin create the admin artifacts it needs when a business
service registers, without patching shenyu. Typical artifacts are plugin
visibility metadata, permissions, selectors, rules and condition rows.

## Contract

Implement `org.apache.shenyu.admin.service.register.ClientRegisterProvisioner`
and expose it as a Spring bean in the admin application.

```java
public final class BcspAuthnProvisioner implements ClientRegisterProvisioner {

    @Override
    public String pluginName() {
        return "bcspAuthn";
    }

    @Override
    public boolean supports(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return "bcspAuthn".equals(metaDataRegisterDTO.getRpcType());
    }

    @Override
    public void provision(final MetaDataRegisterDTO metaDataRegisterDTO) {
        // idempotently create the plugin-specific admin resources
    }
}
```

`supports()` defaults to `true`, so it should filter by rpcType, namespace,
context path or any other dimension the plugin needs. `pluginName()` is used
only for diagnostics.

The hook runs on the register-center disruptor thread and may run concurrently
for different registrations. Implementations must therefore be idempotent,
thread-safe and fast. Do not block on remote calls, long database transactions
or client SDK initialization. When a matching
`ShenyuClientRegisterService` exists, the hook runs after built-in registration
returns successfully. If built-in registration throws, the hook does not run.
When no built-in service is registered for the rpcType, the hook still runs.

Provisioner failures, including class-loading failures such as
`LinkageError`, are logged and isolated from built-in registration and from the
remaining provisioners.

## Registering a third-party jar

ShenYu admin is a plain Spring Boot application and scans only the
`org.apache.shenyu.admin` package by default. A third-party jar must therefore
register its implementation in one of the following ways.

### Spring Boot auto-configuration

Ship a configuration class and list it in
`META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports`.

```java
@AutoConfiguration
public class BcspAuthnProvisionerConfiguration {

    @Bean
    public ClientRegisterProvisioner bcspAuthnProvisioner() {
        return new BcspAuthnProvisioner();
    }
}
```

```text
org.example.authn.BcspAuthnProvisionerConfiguration
```

### Explicit component scan

If the admin deployment owns its bootstrap entry point, it can add the plugin
package to component scanning:

```java
@SpringBootApplication(scanBasePackages = {"org.apache.shenyu", "org.example.authn"})
public class CustomAdminBootstrap {
}
```

Putting a `@Component` implementation in an unscanned package silently no-ops.
Prefer auto-configuration for a distributable admin plugin jar.

Welcome to Apache ShenYu
===============================================================================

Apache ShenYu is an asynchronous, high-performance, cross-language, responsive API gateway.

This package is the minimal HTTP gateway distribution (shenyu-bootstrap-http):
it bundles only what an HTTP reverse-proxy gateway needs.

* HTTP proxy via the divide plugin (plus the core plugin chain: global,
  context-path, uri, transform, response, httpclient).
* Exactly one data-sync channel: websocket (the admin default).
* No RPC protocol proxies (Dubbo, gRPC, Sofa, Tars...), no AI/MCP plugins,
  no log-broker plugins, no alternative sync channels (ZooKeeper, Nacos,
  etcd, Apollo, http long-polling).
* Any removed capability can be added back by declaring the matching
  shenyu-spring-boot-starter-plugin-* (or sync-data) dependency; see the
  shenyu-bootstrap-http module README.

Getting Started
===============================================================================
To help you get started, try the following links:

Getting Started
    https://shenyu.apache.org/docs/quick-start/quick-start-http

We welcome contributions of all kinds, for details of how you can help
    https://shenyu.apache.org/community/contributor/

Find the issue tracker from here
    https://github.com/apache/shenyu/issues

Please help us make Apache ShenYu better - we appreciate any feedback you may have.

Have fun!

-----------------

Licensing
===============================================================================

This software is licensed under the terms you may find in the file named "LICENSE" in this directory.

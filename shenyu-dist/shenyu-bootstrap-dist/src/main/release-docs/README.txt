Welcome to Apache ShenYu
===============================================================================

Apache ShenYu is an asynchronous, high-performance, cross-language, responsive API gateway.

* Support various languages (http protocol), support Dubbo, Spring-Cloud, Grpc and other bundled protocols.
* Sofa and Tars gateway plugins are no longer bundled in the default bootstrap distribution; install the matching external plugin-store starter in the gateway classpath when those protocols are needed.
* Plugin design idea, plugin hot swap, easy to expand.
* Flexible flow filtering to meet various flow control.
* Built-in rich plugin support, authentication, limiting, fuse, firewall, etc.
* Dynamic flow configuration, high performance.
* Support cluster deployment, A/B Test, blue-green release.

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

This convenience binary includes unmodified third-party libraries under terms
that differ from Apache 2.0, including Logback (EPL 1.0, https://logback.qos.ch/),
GlassFish JSON APIs and Parsson (EPL 2.0, https://github.com/eclipse-ee4j/), and
Java annotation APIs (CDDL, https://github.com/javaee/common-annotations-spec).
Snappy JNI includes a statically linked GCC runtime under GPL 3.0 with the GCC
Runtime Library Exception 3.1 (https://www.gnu.org/licenses/gcc-exception-3.1.html).
The LICENSE file identifies exact components and versions; full terms and
copyright notices, including native compression libraries, are in the licenses
directory. Source is available from the linked upstream projects and Maven
Central source artifacts. These libraries are not included as source code in
the ShenYu source distribution.

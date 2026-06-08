E2E compose readiness fix progress

Done:
- Created isolated worktree at .worktrees/fix-e2e-compose-readiness on branch codex/fix-e2e-compose-readiness.
- Confirmed the CI failure path: shenyu-admin briefly failed to connect to MySQL, docker compose returned with shenyu-admin unhealthy, shenyu-bootstrap never became available on localhost:31195, and tests continued until Maven reported gateway unavailable.
- Updated sync compose MySQL healthchecks to verify TCP readiness on 127.0.0.1:3306 instead of relying on the local socket/default transport.
- Updated the HTTP E2E compose script to tolerate a transient compose dependency-health failure, explicitly wait for admin, start bootstrap again, wait for gateway, and log compose services on startup/test failures.
- Verified Bash syntax with bash -n.
- Verified docker compose config parsing for websocket/http/zookeeper sync compose files.
- Attempted a full local HTTP compose E2E run, but Docker image pulls stayed in the pulling phase for more than 6 minutes and never reached container startup; interrupted and cleaned up. No shenyu-* containers or shenyu network remain.

Next:
- Review whether the same startup helper should be shared by the other duplicated compose E2E scripts in a follow-up.
- Full E2E runtime validation still needs CI or a local Docker run with the required images already available.

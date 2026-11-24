# Divide response-body idle timeout

## Rule configuration

A Divide rule may specify an integer `streamIdleTimeout` of 1 through 300000
milliseconds. Admin rejects invalid values before accepting the rule. A missing
or null value retains the existing global timeouts. The gateway captures the
matched value for the lifetime of the request.

## Response receipt

Both built-in HTTP clients apply the rule to every content type. Existing
header timeouts apply until final response headers arrive. During the body,
the idle interval replaces global read and response timeouts and restarts on
nonempty data. HTTPRoute attempt and request deadlines retain their budgets.

Expiry cancels upstream work, logs the timeout and returns HTTP 504 if the
response is uncommitted. A committed response ends with an error. Neither
case retries the request. Completion, error and cancellation release the
request's policy before HTTP/1.1 pool reuse. On HTTP/2, each stream owns its
body timeout. A physical connection read timeout preserves opted-in bodies
while terminating unprotected streams sharing that connection.

[Issue #8455](https://github.com/apache/shenyu/issues/8455)

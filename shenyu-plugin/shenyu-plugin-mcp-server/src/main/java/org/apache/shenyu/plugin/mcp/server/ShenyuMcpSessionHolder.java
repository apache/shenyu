package org.apache.shenyu.plugin.mcp.server;

public class ShenyuMcpSessionHolder {
  private static final ThreadLocal<String> SESSION_ID_HOLDER = new ThreadLocal<>();

  private ShenyuMcpSessionHolder() {
  }

  public static void setSessionId(final String sessionId) {
    SESSION_ID_HOLDER.set(sessionId);
  }

  public static String getSessionId() {
    return SESSION_ID_HOLDER.get();
  }

  public static void clear() {
    SESSION_ID_HOLDER.remove();
  }
}

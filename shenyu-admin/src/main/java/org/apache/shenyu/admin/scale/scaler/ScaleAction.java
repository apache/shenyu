package org.apache.shenyu.admin.scale.scaler;

public class ScaleAction {
    // 定义扩缩容的操作类型
    public enum ActionType {
        SCALE_UP,   // 扩容
        SCALE_DOWN  // 缩容
    }

    private ActionType actionType;   // 扩缩容操作的类型
    private int replicaCount;        // 目标副本数量

    // 构造函数
    public ScaleAction(ActionType actionType, int replicaCount) {
        this.actionType = actionType;
        this.replicaCount = replicaCount;
    }

    // 获取操作类型
    public ActionType getActionType() {
        return actionType;
    }

    // 获取目标副本数
    public int getReplicaCount() {
        return replicaCount;
    }

    // 打印扩缩容操作的信息
    @Override
    public String toString() {
        return String.format("Action: %s, Target Replicas: %d", actionType, replicaCount);
    }
}

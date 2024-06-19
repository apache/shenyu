package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.vo.NamespaceVO;

import java.util.Optional;

public enum NamespaceTransfer {
    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * mapToVo.
     *
     * @param namespaceDO namespaceDO
     * @return NamespaceVO
     */
    public NamespaceVO mapToVo(NamespaceDO namespaceDO) {
        return Optional.ofNullable(namespaceDO).map(data -> {
            NamespaceVO vo = new NamespaceVO();
            vo.setId(data.getId());
            vo.setNamespaceId(data.getNamespaceId());
            vo.setName(data.getName());
            vo.setDescription(data.getDescription());
            return vo;
        }).orElse(null);
    }
}

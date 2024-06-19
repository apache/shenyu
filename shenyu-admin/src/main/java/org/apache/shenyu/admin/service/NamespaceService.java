package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.model.vo.PluginVO;

import java.util.List;

public interface NamespaceService {

    /**
     * Create or update namespace.
     *
     * @param namespaceDTO the namespace dto
     * @return the string
     */
    NamespaceVO createOrUpdate(NamespaceDTO namespaceDTO);

    /**
     * find page of namespace by query.
     *
     * @param namespaceQuery {@linkplain NamespaceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<NamespaceVO> listByPage(NamespaceQuery namespaceQuery);

    /**
     * delete by id.
     *
     * @param namespaceId namespaceId
     * @return msg
     */
    String delete(String namespaceId);

    /**
     * find namespace by id.
     *
     * @param namespaceId pk.
     * @return {@linkplain NamespaceVO}
     */
    NamespaceVO findById(String namespaceId);

    /**
     * find list of namespace.
     *
     * @return {@linkplain List}
     */
    List<NamespaceVO> list();
}

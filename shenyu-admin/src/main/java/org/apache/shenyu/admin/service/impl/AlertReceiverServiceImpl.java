package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.AlertReceiverMapper;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.service.AlertReceiverService;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;

/**
 * Implementation of the {@link AlertReceiverService}.
 */
@Service
public class AlertReceiverServiceImpl implements AlertReceiverService {
	
	@Autowired
	private AlertReceiverMapper alertReceiverMapper;
	
	@Override
	public int addReceiver(AlertReceiverDTO alertReceiverDTO) {
		AlertReceiverDO receiverDO = new AlertReceiverDO();
		BeanUtils.copyProperties(alertReceiverDTO, receiverDO);
		receiverDO.setId(UUIDUtils.getInstance().generateShortUuid());
		Timestamp currentTime = new Timestamp(System.currentTimeMillis());
		receiverDO.setDateCreated(currentTime);
		receiverDO.setDateUpdated(currentTime);
		return alertReceiverMapper.insert(receiverDO);
	}
	
	@Override
	public int deleteReceiver(List<String> ids) {
		return alertReceiverMapper.deleteByIds(ids);
	}
	
	@Override
	public int updateReceiver(AlertReceiverDTO alertReceiverDTO) {
		AlertReceiverDO receiverDO = new AlertReceiverDO();
		BeanUtils.copyProperties(alertReceiverDTO, receiverDO);
		return alertReceiverMapper.updateByPrimaryKey(receiverDO);
	}
	
	@Override
	public List<AlertReceiverDTO> getAll() {
		return alertReceiverMapper.selectAll();
	}
	
	@Override
	public AlertReceiverDTO detail(String id) {
		AlertReceiverDTO receiverDTO = new AlertReceiverDTO();
		AlertReceiverDO receiverDO = alertReceiverMapper.selectByPrimaryKey(id);
		if (receiverDO != null) {
			BeanUtils.copyProperties(receiverDO, receiverDTO);
			return receiverDTO;
		} else {
			return null;
		}
	}
}

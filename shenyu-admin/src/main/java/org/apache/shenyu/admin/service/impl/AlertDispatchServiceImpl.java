package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import org.apache.shenyu.admin.mapper.AlertReceiverMapper;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.alert.AlertNotifyHandler;
import org.apache.shenyu.alert.exception.AlertNoticeException;
import org.apache.shenyu.alert.model.AlertContentDTO;
import org.apache.shenyu.admin.service.AlertDispatchService;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.AlertDispatchService}.
 * @author tom
 * @date 2023/6/27 15:14
 */
@Service
public class AlertDispatchServiceImpl implements AlertDispatchService, InitializingBean {
	
	private static final int DISPATCH_THREADS = 3;
	
	private final LinkedBlockingQueue<AlertContentDTO> alertDataQueue;
	
	private final Map<Byte, AlertNotifyHandler> alertNotifyHandlerMap;
	
	private final AlertReceiverMapper alertReceiverMapper;
	
	private final AtomicReference<List<AlertReceiverDTO>> alertReceiverReference;
	
	private static final Logger log = LoggerFactory.getLogger(AlertDispatchServiceImpl.class);
	
	public AlertDispatchServiceImpl(List<AlertNotifyHandler> alertNotifyHandlerList, AlertReceiverMapper alertReceiverMapper) {
		this.alertDataQueue = new LinkedBlockingQueue<>();
		this.alertReceiverMapper = alertReceiverMapper;
		this.alertReceiverReference = new AtomicReference<>();
		alertNotifyHandlerMap = Maps.newHashMapWithExpectedSize(alertNotifyHandlerList.size());
		alertNotifyHandlerList.forEach(r -> alertNotifyHandlerMap.put(r.type(), r));
	}
	
	@Override
	public void afterPropertiesSet() throws Exception {
		ThreadFactory threadFactory = new ThreadFactoryBuilder()
				                              .setUncaughtExceptionHandler((thread, throwable) -> {
					                              log.error("workerExecutor has uncaughtException.");
					                              log.error(throwable.getMessage(), throwable); })
				                              .setDaemon(true)
				                              .setNameFormat("alerter-worker-%d")
				                              .build();
		ThreadPoolExecutor workerExecutor = new ThreadPoolExecutor(3,
				3,
				10,
				TimeUnit.SECONDS,
				new SynchronousQueue<>(),
				threadFactory,
				new ThreadPoolExecutor.AbortPolicy());
		DispatchTask dispatchTask = new DispatchTask();
		for (int i = 0; i < DISPATCH_THREADS; i++) {
			workerExecutor.execute(dispatchTask);
		}
	}
	
	@Override
	public void dispatchAlert(AlertContentDTO alertContentDTO) {
		alertDataQueue.offer(alertContentDTO);
	}
	
	@Override
	public void clearCache() {
		this.alertReceiverReference.set(null);
	}
	
	
	private class DispatchTask implements Runnable {
		@Override
		public void run() {
			while (!Thread.currentThread().isInterrupted()) {
				try {
					AlertContentDTO alert = alertDataQueue.poll();
					if (alert != null) {
						sendNotify(alert);
					}
				} catch (Exception e) {
					log.error(e.getMessage());
				}
			}
		}
		
		private void sendNotify(AlertContentDTO alert) {
			// Forward configured email WeChat webhook
			List<AlertReceiverDTO> receivers = matchReceiverByRules(alert);
			for (AlertReceiverDTO receiver : receivers) {
				try {
					sendNoticeMsg(receiver, alert);
				} catch (AlertNoticeException e) {
					log.warn("DispatchTask sendNoticeMsg error, message: {}", e.getMessage());
				}
			}
		}
		
		private List<AlertReceiverDTO> matchReceiverByRules(AlertContentDTO alert) {
			List<AlertReceiverDTO> dtoList = alertReceiverReference.get();
			if (dtoList == null) {
				List<AlertReceiverDO> receiverDOList = alertReceiverMapper.selectAll();
				dtoList = receiverDOList.stream().map(item -> {
					AlertReceiverDTO receiverDTO = new AlertReceiverDTO();
					BeanUtils.copyProperties(item, receiverDTO);
					return receiverDTO;
				}).collect(Collectors.toList());
				alertReceiverReference.set(dtoList);
			}
			return dtoList.stream().filter(item -> {
				if (item.isEnable()) {
					if (item.getLabels() != null && !item.getLabels().isEmpty()) {
						return item.getLabels().entrySet().stream().anyMatch(entry -> {
							if (alert.getLabels() == null || !alert.getLabels().containsKey(entry.getKey())) {
								return false;
							}
							String labelValue = alert.getLabels().get(entry.getKey());
							return Objects.equals(labelValue, entry.getValue());
						});
					}
					return true;
				} else {
					return false;
				}
			}).collect(Collectors.toList());
		}
		
		private void sendNoticeMsg(AlertReceiverDTO receiver, AlertContentDTO alert) {
			if (receiver == null || receiver.getType() == null) {
				log.warn("DispatcherAlarm-sendNoticeMsg params is empty alert:[{}], receiver:[{}]", alert, receiver);
				return;
			}
			byte type = receiver.getType();
			if (alertNotifyHandlerMap.containsKey(type)) {
				alertNotifyHandlerMap.get(type).send(receiver, alert);
			}
		}
	}
}

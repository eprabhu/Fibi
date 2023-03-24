package com.polus.fibicomp.reminder.service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.reminder.dao.ReminderDao;
import com.polus.fibicomp.reminder.pojo.ReminderNotification;

@Component
@Transactional
public class ReminderScheduler {

	protected static Logger logger = LogManager.getLogger(ReminderScheduler.class.getName());
	
	@Autowired
	private ReminderDao reminderDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private CommonService commonService;

	@Scheduled(cron = "${reminder.notification.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendReminderNotifiaction() {
		logger.info("Scheduler for Reminder Notification started at : {}", commonDao.getCurrentTimestamp());
		List<ReminderNotification> reminders = reminderDao.fetchAllActiveReminders();
		reminders.forEach(this::getThingsToRemind);
	}

	private void getThingsToRemind(ReminderNotification reminder) {
		String personId = null;
		ResultSet rs = reminderDao.getReminderData(reminder.getDaysToDueDate(), reminder.getProcedureName());
		try {
			while (rs.next()) {
				Integer moduleCode = rs.getInt("MODULE_CODE");
				Integer subModuleCode = rs.getInt("SUB_MODULE_CODE");
				String moduleItemKey = rs.getString("MODULE_ITEM_KEY");
				String subModuleItemKey = rs.getString("SUB_MODULE_ITEM_KEY");
				try {
					personId = rs.getString("PERSON_ID");
				} catch (SQLException e) {
					personId = null;
				}
				Map<String, String> placeHolder = new HashMap<>();
				if (reminder.getPlaceHolderValues() != null && !reminder.getPlaceHolderValues().equals("")) {
					List<String> indexKeys = Arrays.asList(reminder.getPlaceHolderValues().split("\\s*,\\s*"));
					placeHolder = indexKeys.stream().collect(Collectors.toMap( key->new StringBuilder("{").append(key).append("}").toString(), key -> {
						try {
							return rs.getString(key) != null ? rs.getString(key) : "";
						} catch (SQLException e) {
							logger.error("Error Occured in getThingsToRemaind due to SQLException: {}", e.getMessage());
							return "";
						}
					}));
				}
				setNotifictionRecipient(reminder.getNotificationId(), personId, moduleCode, subModuleCode, moduleItemKey, subModuleItemKey, placeHolder);
			}
		} catch (Exception e) {
			logger.error("Error Occured in getThingsToRemaind : {}", e.getMessage());
		}
	}

	private void setNotifictionRecipient(Integer notificationId, String personId, Integer moduleCode, Integer subModuleCode, String moduleItemKey, String subModuleItemKey, Map<String, String> placeHolder) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (personId != null) {
			commonService.setNotificationRecipients(personId, "TO", dynamicEmailRecipients);
			sendNotification(notificationId, moduleCode, subModuleCode, moduleItemKey, subModuleItemKey, placeHolder, dynamicEmailRecipients);
		} else {
			sendNotification(notificationId, moduleCode, subModuleCode, moduleItemKey, subModuleItemKey, placeHolder, null);
		}
	}

	public void sendNotification(Integer notificationTypeId, Integer moduleCode, Integer subModuleCode, String moduleItemKey, String subModuleItemKey, Map<String, String> placeHolder, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(moduleCode);
		emailServiceVO.setModuleItemKey(moduleItemKey);
		emailServiceVO.setSubModuleCode(subModuleCode.toString());
		emailServiceVO.setSubModuleItemKey(subModuleItemKey);
		emailServiceVO.setPlaceHolder(placeHolder);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);	
	}
}

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
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.ConflictOfInterestDao;
import com.polus.fibicomp.coi.service.ConflictOfInterestService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.reminder.dao.ReminderDao;
import com.polus.fibicomp.reminder.pojo.ReminderNotification;
import com.polus.fibicomp.security.AuthenticatedUser;


@Component
@Transactional
@Service
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
	
	@Autowired
	private InboxDao inboxDao;
	
	@Autowired
	@Qualifier(value = "conflictOfInterestDao")
	private ConflictOfInterestDao conflictOfInterestDao;

	@Scheduled(cron = "${reminder.notification.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendReminderNotifiaction() {
		logger.info("Scheduler for Reminder Notification started at : {}", commonDao.getCurrentTimestamp());
		List<ReminderNotification> reminders = reminderDao.fetchAllActiveReminders();
		reminders.forEach(this::getThingsToRemind);
	}

	private void getThingsToRemind(ReminderNotification reminder) {
		String personId = null;
		ResultSet rs = reminderDao.getReminderData(reminder.getDaysToDueDate(), reminder.getProcedureName());
		if (reminder.getReminderTypeFlag() == null || reminder.getReminderTypeFlag() == "N") {
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
		} else {
			this.getThingsForBanner(reminder, rs);
		}
		
	}
	
	/* This method will be invoked for `ALERT_TYPE` = "B" means for Banners.To get things work for banners, we added a new column- `REMINDER_TYPE_FLAG` to the
	 * existing table `REMINDER_NOTIFICATION` and set the value as "B" for notification Banners. The method here will lists out different persons with expiration
	 * warning(For instance 365 days) for the disclosures. Then will add entries corresponding to each person in the action list table. So that if any user comes
	 * and login to the application and tries to click on the Notification icon, they can see what are the disclosures which are going to expire in the given
	 * number of days.
	*/
	private void getThingsForBanner(ReminderNotification reminder, ResultSet rs) {
			try {
				while (rs.next()) {
					List<String> personIds = Arrays.asList(rs.getString("PERSON_ID").split("\\s*,\\s*"));
					if (!personIds.isEmpty()) {
						personIds.forEach(id -> {
							saveBannerNotificationsToActionListForPerson(reminder, rs, id);
						});
					}
				}
			} catch (Exception e) {
				logger.error("Error Occured in getThingsForBanner : {}", e.getMessage());
			}
	}
	
	private void saveBannerNotificationsToActionListForPerson(ReminderNotification reminder, ResultSet rs, String personId) {
		try {
			Inbox actionList = new Inbox();
			actionList.setToPersonId(personId);
			actionList.setModuleCode(rs.getInt("MODULE_CODE"));
			actionList.setSubModuleCode(rs.getInt("SUB_MODULE_CODE"));
			actionList.setModuleItemKey(rs.getString("MODULE_ITEM_KEY"));
			actionList.setSubModuleItemKey(rs.getString("SUB_MODULE_ITEM_KEY"));
			actionList.setExpirationDate(rs.getTimestamp("EXPIRATION_DATE"));
			actionList.setAlertType(reminder.getReminderTypeFlag());
			actionList.setUpdateUser(rs.getString("UPDATE_USER"));
			actionList.setUpdateTimeStamp(rs.getTimestamp("UPDATE_TIMESTAMP"));
			actionList.setUserMessage(rs.getString("USER_MESSAGE"));
			actionList.setMessageTypeCode(rs.getString("EXPIRING_MESSAGE_TYPE_CODE"));
			inboxDao.saveBannerEntriesToActionList(actionList);
		} catch (SQLException e) {
			logger.error("Error Occured in getThingsForBanner : {}", e.getMessage());
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

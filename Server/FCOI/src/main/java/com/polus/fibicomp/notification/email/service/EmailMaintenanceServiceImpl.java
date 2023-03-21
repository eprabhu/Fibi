package com.polus.fibicomp.notification.email.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.email.vo.EmailMaintenanceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.pojo.PersonPreference;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "emailMaintenanceService")
public class EmailMaintenanceServiceImpl implements EmailMaintenanceService {

	protected static Logger logger = LogManager.getLogger(EmailMaintenanceServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "emailMaintenanceDao")
	private EmailMaintenanceDao emailMaintenanceDao;
	
	@Autowired
	private CommonDao commonDao;

	@Value("${spring.mail.username}")
	private String username;

	@Override
	public String saveOrUpdateNotification(EmailMaintenanceVO vo) {
		NotificationType notificationType = vo.getNotificationType();
		notificationType = emailMaintenanceDao.saveOrUpdateNotificationType(notificationType);
		vo.setNotificationType(notificationType);
		if (notificationType.getIsSystemSpecific().equals("Y")) {
			emailMaintenanceDao.deleteUserNotificationById(notificationType.getNotificationTypeId(), vo.getPersonId());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAllNotification(EmailMaintenanceVO vo) {
		vo = emailMaintenanceDao.fetchAllNotifications(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String removeNotificationRecipient(EmailMaintenanceVO vo) {
		NotificationRecipient notificationRecipient = emailMaintenanceDao
				.fetchNotificationRecipient(vo.getNotificationRecipientId());
		notificationRecipient = emailMaintenanceDao.removeNotificationRecipient(notificationRecipient);
		return commonDao.convertObjectToJSON(notificationRecipient);
	}

	@Override
	public String getNotificationById(EmailMaintenanceVO vo) {
		NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(vo.getNotificationTypeId());
		vo.setNotificationType(notificationType);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String removeNotificationById(EmailMaintenanceVO vo) {
		NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(vo.getNotificationTypeId());
		emailMaintenanceDao.removeNotificationType(notificationType);
		emailMaintenanceDao.removePersonPreferenceByNotificationTypeId(vo.getNotificationTypeId());
		vo.setMessage("Successfully Deactivated");
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchUserNotification(String personId) {
		EmailMaintenanceVO vo = new EmailMaintenanceVO();
		vo.setPersonId(personId);
		HashMap<String, List<HashMap<String, Object>>> notification = new HashMap<>();
		List<HashMap<String, Object>> getUserNotification = emailMaintenanceDao.fetchNotificationType(vo);
		for (int i = 0; i < getUserNotification.size(); i++) {
			List<HashMap<String, Object>> notificationList = new ArrayList<>();
			for (int j = 0; j < getUserNotification.size(); j++) {
				if (getUserNotification.get(i).get("MODULE_CODE").equals(getUserNotification.get(j).get("MODULE_CODE"))) {
					notificationList.add(getUserNotification.get(j));
				}
			}
			notification.put(getUserNotification.get(i).get("MODULE_DESCRIPTION").toString(), notificationList);
		}
		return commonDao.convertObjectToJSON(notification);
	}

	@Override
	public String activateOrDeactivateUserNotification(EmailMaintenanceVO vo) {
		if (vo.getPreferenceId() != null) {
			emailMaintenanceDao.activateUserNotification(vo.getPreferenceId());
			vo.setActivated(true);
		} else {
			PersonPreference personPreference = new PersonPreference();
			personPreference.setPersonId(AuthenticatedUser.getLoginPersonId());
			personPreference.setUpdateUser(AuthenticatedUser.getLoginUserName());
			personPreference.setPreferencesTypeCode(Constants.PREFERENCE_TYPE_CODE);
			personPreference.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			personPreference.setValue(vo.getNotificationTypeCode());
			emailMaintenanceDao.deactivateUserNotification(personPreference);
			vo.setPreferenceId(personPreference.getPreferenceId());
			vo.setActivated(false);
		}
		return commonDao.convertObjectToJSON(vo);
	}

}

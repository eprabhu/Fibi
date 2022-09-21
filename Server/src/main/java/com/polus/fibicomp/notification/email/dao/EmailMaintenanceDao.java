package com.polus.fibicomp.notification.email.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.notification.email.vo.EmailMaintenanceVO;
import com.polus.fibicomp.notification.pojo.NotificationLog;
import com.polus.fibicomp.notification.pojo.NotificationLogRecipient;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.pojo.PersonPreference;

@Transactional
@Service
public interface EmailMaintenanceDao {

	/**
	 * This method is used to save or update the notification type
	 * @param notificationType
	 * @return object of updated notification type
	 */
	public  NotificationType saveOrUpdateNotificationType(NotificationType notificationType);

	/**
	 * This method is used to fetch all the notification type
	 * @param vo
	 * @return list of notification types
	 */
	public EmailMaintenanceVO fetchAllNotifications(EmailMaintenanceVO vo);

	/**
	 * This method is used to fetch notification by id
	 * @param notificationTypeId
	 * @return notification details based on id
	 */
	public NotificationType fetchNotificationById(Integer notificationTypeId);

	/**
	 * This method is used to remove the notification recipient
	 * @param notificationRecipient
	 * @return removed recipient
	 */
	public NotificationRecipient removeNotificationRecipient(NotificationRecipient notificationRecipient);

	/**
	 * This method is used to fetch notification recipient by id
	 * @param notificationRecipientId
	 * @return notification recipient
	 */
	public NotificationRecipient fetchNotificationRecipient(Integer notificationRecipientId);

	/**
	 * This method is used to remove the notification type
	 * @param notificationType
	 * @return removed notification type
	 */
	public NotificationType removeNotificationType(NotificationType notificationType);

	/**
	 * This method is used to fetch the recipient details based on role code
	 * @param roleTypeCode
	 * @param recipientType
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param subModuleCode
	 * @param subModuleItemKey
	 * @return details with person id, email, name
	 */
	public Set<NotificationRecipient> getRoleEmail(Integer roleTypeCode, String recipientType, Integer moduleCode, String moduleItemKey, Integer notificationTypeId, Integer subModuleCode, String subModuleItemKey);

	/**
	 * This method is used to log the mail details in notification log table
	 * @param notificationLog
	 */
	public NotificationLog createNotificationLog(NotificationLog notificationLog);

	/**
	 * This method is used to fetch the email and person preference flag
	 * @param recipientPersonId
	 * @return email and flag
	 */
	public NotificationLogRecipient getEmailAndFlag(String recipientPersonId, Integer notificationType);

	/**
	 * This method is used to save or update the notification log recipient
	 * @param notificationLogRecipient
	 */
	public void saveOrUpdate(NotificationLogRecipient notificationLogRecipient);
	

	/** This method is used to fetch notifications list
	 * @param emailMaintenanceVO
	 * @return list of notifications
	 */
	public ArrayList<HashMap<String, Object>> fetchNotificationType(EmailMaintenanceVO emailMaintenanceVO);
	
	/**
	 * This method is used to insert record to person preference table when user disable notification.
	 * @param personPreference
	 */
	public void deactivateUserNotification(PersonPreference personPreference);
	
	/**
	 * This method is used to delete record from person preference table.
	 * @param personPreference
	 */
	public void activateUserNotification(Integer preferenceId);
	
	/**
	 * This method is used to delete record from person preference table when notification type change to Y.
	 * @param emailMaintenanceVO
	 * @return success message
	 */
	public void deleteUserNotificationById(Integer notificationTypeId, String personid);

	/**
	 * This method is used to delete the person preferences for the given notification type id
	 * @param notificationTypeId
	 */
	public void removePersonPreferenceByNotificationTypeId(Integer notificationTypeId);

}

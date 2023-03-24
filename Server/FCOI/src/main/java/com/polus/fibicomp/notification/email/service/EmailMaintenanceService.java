package com.polus.fibicomp.notification.email.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.notification.email.vo.EmailMaintenanceVO;

@Transactional
@Service
public interface EmailMaintenanceService {

	/**
	 * This method is used to save or update notification details
	 * @param vo
	 * @return created or updated object
	 */
	public String saveOrUpdateNotification(EmailMaintenanceVO vo);

	/**
	 * This method is used to remove the notification recipient by id
	 * @param vo
	 * @return removed recipient
	 */
	public String removeNotificationRecipient(EmailMaintenanceVO vo);

	/**
	 * This method is used to fetch all the notifications
	 * @param vo
	 * @return list of notification types
	 */
	public String getAllNotification(EmailMaintenanceVO vo);

	/**
	 * This method is used to get the notification details by notification type id
	 * @param vo
	 * @return details of notification type
	 */
	public String getNotificationById(EmailMaintenanceVO vo);

	/**
	 * This method is used to remove the notification type by id
	 * @param vo
	 * @return removed notification type
	 */
	public String removeNotificationById(EmailMaintenanceVO vo);

//	/**
//	 * This method is used to fetch all the fibi persons 
//	 * @param vo
//	 * @return list of fibi persons
//	 */
//	public String getFibiPersons(EmailMaintenanceVO vo);

	/**
	 * This method is used for fetch User Notifications.
	 * @param personId
	 * @return list of user notification
	 */
	public String fetchUserNotification(String personId);

	/**
	 * This method is used for activate or deactivate User Notifications.
	 * @param vo
	 * @return success message
	 */
	public String activateOrDeactivateUserNotification(EmailMaintenanceVO vo);

}

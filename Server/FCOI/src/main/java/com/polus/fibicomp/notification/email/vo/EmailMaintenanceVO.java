package com.polus.fibicomp.notification.email.vo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.pojo.Person;

public class EmailMaintenanceVO {
	private NotificationType notificationType;
	private Integer notificationTypeId;
	private Integer notificationRecipientId;
	private NotificationRecipient notificationRecipient;
	private Integer currentPage;
	private Integer pageNumber;
	private Integer totalNotifications;
	private List<NotificationType> notificationTypes;
	private List<Person> personDetails;
	private String moduleItemKey;
	private String moduleCode;
	private String message;
	private String subject;
	private List<NotificationRecipient> emailRecipient;
	private String personId;
	private Integer preferenceId;
	private String notificationTypeCode;
	private String updateUser;
	private Map<String, String> sort = new HashMap<>();
	private Boolean activated;

	public NotificationType getNotificationType() {
		return notificationType;
	}

	public void setNotificationType(NotificationType notificationType) {
		this.notificationType = notificationType;
	}

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(String notificationTypeId) {
		Integer id = Integer.parseInt(notificationTypeId);
		this.notificationTypeId = id;
	}

	public Integer getNotificationRecipientId() {
		return notificationRecipientId;
	}

	public void setNotificationRecipientId(String notificationRecipientId) {
		Integer id = Integer.parseInt(notificationRecipientId);
		this.notificationRecipientId = id;
	}

	public NotificationRecipient getNotificationRecipient() {
		return notificationRecipient;
	}

	public void setNotificationRecipient(NotificationRecipient notificationRecipient) {
		this.notificationRecipient = notificationRecipient;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(String currentPage) {
		Integer number = Integer.parseInt(currentPage);
		this.currentPage = number;
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(String pageNumber) {
		Integer number = Integer.parseInt(pageNumber);
		this.pageNumber = number;
	}

	public Integer getTotalNotifications() {
		return totalNotifications;
	}

	public void setTotalNotifications(Integer totalNotifications) {
		this.totalNotifications = totalNotifications;
	}

	public List<NotificationType> getNotificationTypes() {
		return notificationTypes;
	}

	public void setNotificationTypes(List<NotificationType> notificationTypes) {
		this.notificationTypes = notificationTypes;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public void setNotificationRecipientId(Integer notificationRecipientId) {
		this.notificationRecipientId = notificationRecipientId;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public List<Person> getPersonDetails() {
		return personDetails;
	}

	public void setPersonDetails(List<Person> personDetails) {
		this.personDetails = personDetails;
	}

	public List<NotificationRecipient> getEmailRecipient() {
		return emailRecipient;
	}

	public void setEmailRecipient(List<NotificationRecipient> emailRecipient) {
		this.emailRecipient = emailRecipient;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(String moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getPreferenceId() {
		return preferenceId;
	}

	public void setPreferenceId(Integer preferenceId) {
		this.preferenceId = preferenceId;
	}

	public String getNotificationTypeCode() {
		return notificationTypeCode;
	}

	public void setNotificationTypeCode(String notificationTypeCode) {
		this.notificationTypeCode = notificationTypeCode;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public Boolean getActivated() {
		return activated;
	}

	public void setActivated(Boolean activated) {
		this.activated = activated;
	}

}

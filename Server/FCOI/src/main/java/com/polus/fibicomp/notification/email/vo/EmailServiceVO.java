package com.polus.fibicomp.notification.email.vo;

import java.io.File;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.vo.NotificationServiceVO;

public class EmailServiceVO extends NotificationServiceVO{

	private Set<NotificationRecipient> recipients;

	private Integer moduleCode;	

	private String moduleItemKey;		

	private Integer notificationTypeId;

	private boolean hasAttachment;

	private String body;

	private String subject;

	private String errorMessage;

	private Boolean prompted;

	private Map<String, String> placeHolder;

	private String subModuleCode;

	private String subModuleItemKey;

	private File fileName;

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public boolean isHasAttachment() {
		return hasAttachment;
	}

	public void setHasAttachment(boolean hasAttachment) {
		this.hasAttachment = hasAttachment;
	}

	public Set<NotificationRecipient> getRecipients() {
		return recipients;
	}

	public void setRecipients(Set<NotificationRecipient> recipients) {
		this.recipients = recipients;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public Boolean getPrompted() {
		return prompted;
	}

	public void setPrompted(Boolean prompted) {
		this.prompted = prompted;
	}

	public Map<String, String> getPlaceHolder() {
		return placeHolder;
	}

	public void setPlaceHolder(Map<String, String> placeHolder) {
		this.placeHolder = placeHolder;
	}

	public String getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(String subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public File getFileName() {
		return fileName;
	}

	public void setFileName(File fileName) {
		this.fileName = fileName;
	}

}

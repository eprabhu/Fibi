package com.polus.fibicomp.notification.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "NOTIFICATION_LOG")

public class NotificationLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "NOTIFICATION_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "NOTIFY_LOG_ID_GENERATOR")
	@SequenceGenerator(name="NOTIFY_LOG_ID_GENERATOR", sequenceName = "NOTIFY_LOG_ID_GENERATOR", allocationSize=1)
	private Integer notificationLogId;

	@Column(name = "NOTIFICATION_TYPE_ID")
	private Integer notificationTypeId;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "FROM_USER_EMAIL_ID")
	private String fromUserEmailId;

	@Column(name = "TO_USER_EMAIL_ID")
	private String toUserEmailId;

	@Column(name = "SUBJECT")
	private String subject;

	@Column(name = "MESSAGE")
	private String message;

	@Column(name = "SEND_DATE")
	private Timestamp sendDate;

	@Column(name = "IS_SUCCESS")
	private String isSuccess;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@OneToMany(mappedBy = "notificationLog", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NotificationLogRecipient> notificationLogRecipients;

	public Integer getNotificationLogId() {
		return notificationLogId;
	}

	public void setNotificationLogId(Integer notificationLogId) {
		this.notificationLogId = notificationLogId;
	}

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getFromUserEmailId() {
		return fromUserEmailId;
	}

	public void setFromUserEmailId(String fromUserEmailId) {
		this.fromUserEmailId = fromUserEmailId;
	}

	public String getToUserEmailId() {
		return toUserEmailId;
	}

	public void setToUserEmailId(String toUserEmailId) {
		this.toUserEmailId = toUserEmailId;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Timestamp getSendDate() {
		return sendDate;
	}

	public void setSendDate(Timestamp sendDate) {
		this.sendDate = sendDate;
	}

	public String getIsSuccess() {
		return isSuccess;
	}

	public void setIsSuccess(String isSuccess) {
		this.isSuccess = isSuccess;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public List<NotificationLogRecipient> getNotificationLogRecipients() {
		return notificationLogRecipients;
	}

	public void setNotificationLogRecipients(List<NotificationLogRecipient> notificationLogRecipients) {
		this.notificationLogRecipients = notificationLogRecipients;
	}

}

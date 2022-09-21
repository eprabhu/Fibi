package com.polus.fibicomp.notification.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "NOTIFICATION_LOG_RECIPIENT")
public class NotificationLogRecipient implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "NOTIFICATION_LOG_RECIPIENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "NOTIFY_LOG_RCPT_ID_GENERATOR")
	@SequenceGenerator(name="NOTIFY_LOG_RCPT_ID_GENERATOR", sequenceName = "NOTIFY_LOG_RCPT_ID_GENERATOR", allocationSize=1)
	private Integer notificationLogRecipientId;

	@Column(name = "NOTIFICATION_LOG_ID")
	private Integer notificationLogId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "NOTIFICATION_LOG_RECIPIENT_FK2"), name = "NOTIFICATION_LOG_ID", referencedColumnName = "NOTIFICATION_LOG_ID", insertable = false, updatable = false)
	private NotificationLog notificationLog;

	@Column(name = "TO_USER_EMAIL_ID")
	private String toUserEmailId;

	@Column(name = "MAIL_SENT_FLAG")
	private String mailSentFlag;

	public Integer getNotificationLogRecipientId() {
		return notificationLogRecipientId;
	}

	public void setNotificationLogRecipientId(Integer notificationLogRecipientId) {
		this.notificationLogRecipientId = notificationLogRecipientId;
	}

	public NotificationLog getNotificationLog() {
		return notificationLog;
	}

	public void setNotificationLog(NotificationLog notificationLog) {
		this.notificationLog = notificationLog;
	}

	public Integer getNotificationLogId() {
		return notificationLogId;
	}

	public void setNotificationLogId(Integer notificationLogId) {
		this.notificationLogId = notificationLogId;
	}

	public String getToUserEmailId() {
		return toUserEmailId;
	}

	public void setToUserEmailId(String toUserEmailId) {
		this.toUserEmailId = toUserEmailId;
	}

	public String getMailSentFlag() {
		return mailSentFlag;
	}

	public void setMailSentFlag(String mailSentFlag) {
		this.mailSentFlag = mailSentFlag;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((toUserEmailId == null) ? 0 : toUserEmailId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		NotificationLogRecipient other = (NotificationLogRecipient) obj;
		if (toUserEmailId == null) {
			if (other.toUserEmailId != null)
				return false;
		} else if (!toUserEmailId.equals(other.toUserEmailId))
			return false;
		return true;
	}

}

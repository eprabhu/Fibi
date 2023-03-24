package com.polus.fibicomp.notification.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "NOTIFICATION_RECIPIENT")
public class NotificationRecipient implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "NOTIFICATION_RECIPIENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "NOTIFY_RCPT_ID_GENERATOR")
	@SequenceGenerator(name="NOTIFY_RCPT_ID_GENERATOR", sequenceName = "NOTIFY_RCPT_ID_GENERATOR", allocationSize=1)
	private Integer notificationRecipientId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "NOTIFICATION_RECIPIENT_FK1"), name = "NOTIFICATION_TYPE_ID", referencedColumnName = "NOTIFICATION_TYPE_ID")
	private NotificationType notificationType;

	@Column(name = "RECIPIENT_PERSON_ID")
	private String recipientPersonId;

	@Column(name = "ROLE_TYPE_CODE")
	private Integer roleTypeCode;

	@Column(name = "RECIPIENT_TYPE")
	private String recipientType;

	@Column(name = "RECIPIENT_NAME")
	private String recipientName;
	
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	private transient String emailAddress;

	private transient String sendFlag;

	public Integer getNotificationRecipientId() {
		return notificationRecipientId;
	}

	public void setNotificationRecipientId(Integer notificationRecipientId) {
		this.notificationRecipientId = notificationRecipientId;
	}

	public NotificationType getNotificationType() {
		return notificationType;
	}

	public void setNotificationType(NotificationType notificationType) {
		this.notificationType = notificationType;
	}

	public String getRecipientPersonId() {
		return recipientPersonId;
	}

	public void setReceipientPersonId(String recipientPersonId) {
		this.recipientPersonId = recipientPersonId;
	}

	public Integer getRoleTypeCode() {
		return roleTypeCode;
	}

	public void setRoleTypeCode(Integer roleTypeCode) {
		this.roleTypeCode = roleTypeCode;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public void setRecipientPersonId(String recipientPersonId) {
		this.recipientPersonId = recipientPersonId;
	}

	public String getRecipientName() {
		return recipientName;
	}

	public void setRecipientName(String recipientName) {
		this.recipientName = recipientName;
	}

	public String getRecipientType() {
		return recipientType;
	}

	public void setRecipientType(String recipientType) {
		this.recipientType = recipientType;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getSendFlag() {
		return sendFlag;
	}

	public void setSendFlag(String sendFlag) {
		this.sendFlag = sendFlag;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((emailAddress == null) ? 0 : emailAddress.hashCode());
		result = prime * result + ((recipientType == null) ? 0 : recipientType.hashCode());
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
		NotificationRecipient other = (NotificationRecipient) obj;
		if (emailAddress == null) {
				return false;
		} else if (!emailAddress.equals(other.emailAddress)) {
			return false;
		}
		if (recipientType == null) {
			if (other.recipientType != null)
				return false;
		} else if (!recipientType.equals(other.recipientType))
			return false;
		return true;
	}

}

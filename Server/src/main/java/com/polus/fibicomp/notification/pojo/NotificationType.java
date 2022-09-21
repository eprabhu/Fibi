package com.polus.fibicomp.notification.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.Module;

@Entity
@Table(name = "NOTIFICATION_TYPE")
public class NotificationType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "notificationIdGenerator", strategy = "com.polus.fibicomp.generator.FibiPKGenerator")
	@GeneratedValue(generator = "notificationIdGenerator")
	@Column(name = "NOTIFICATION_TYPE_ID")
	private Integer notificationTypeId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "NOTIFICATION_TYPE_FK1"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module module;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "SUBJECT")
	private String subject;

	@Column(name = "MESSAGE")
	private String message;

	@Column(name = "PROMPT_USER")
	private String promptUser;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "notificationType", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<NotificationRecipient> notificationRecipient;
	
	@Column(name = "IS_SYSTEM_SPECIFIC")
	private String isSystemSpecific;
	
	public NotificationType() {
		notificationRecipient = new ArrayList<NotificationRecipient>();
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

	public Module getModule() {
		return module;
	}

	public void setModule(Module module) {
		this.module = module;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public String getPromptUser() {
		return promptUser;
	}

	public void setPromptUser(String promptUser) {
		this.promptUser = promptUser;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
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

	public List<NotificationRecipient> getNotificationRecipient() {
		return notificationRecipient;
	}

	public void setNotificationRecipient(List<NotificationRecipient> notificationRecipient) {
		this.notificationRecipient = notificationRecipient;
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

	public String getIsSystemSpecific() {
		return isSystemSpecific;
	}

	public void setIsSystemSpecific(String isSystemSpecific) {
		this.isSystemSpecific = isSystemSpecific;
	}
	
}

package com.polus.fibicomp.inbox.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Module;

@Entity
@Table(name = "INBOX")
public class Inbox implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "INBOX_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "INBOX_ID_GENERATOR")
	@SequenceGenerator(name="INBOX_ID_GENERATOR", sequenceName = "INBOX_ID_GENERATOR", allocationSize=1)
	private Integer inboxId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MESSAGE_TYPE_CODE")
	private String messageTypeCode;

	@ManyToOne(optional = true, cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "INBOX_FK1"), name = "MESSAGE_TYPE_CODE", referencedColumnName = "MESSAGE_TYPE_CODE", insertable = false, updatable = false)
	private Message message;

	@Column(name = "USER_MESSAGE")
	private String userMessage;

	@Column(name = "OPENED_FLAG")
	private String openedFlag;

	@Column(name = "SUBJECT_TYPE")
	private String subjectType;

	@Column(name = "TO_PERSON_ID")
	private String toPersonId;

	@Column(name = "ARRIVAL_DATE")
	private Timestamp arrivalDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "SUB_MODULE_ITEM_KEY")
	private String subModuleItemKey;

	@Column(name = "ALERT_TYPE")
	private String alertType;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@ManyToOne(optional = true, cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "INBOX_FK2"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module moduleName;

	public Integer getInboxId() {
		return inboxId;
	}

	public void setInboxId(Integer inboxId) {
		this.inboxId = inboxId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getSubjectType() {
		return subjectType;
	}

	public void setSubjectType(String subjectType) {
		this.subjectType = subjectType;
	}

	public String getToUser() {
		return toPersonId;
	}

	public void setToUser(String toUser) {
		this.toPersonId = toUser;
	}

	public Timestamp getArrivalDate() {
		return arrivalDate;
	}

	public void setArrivalDate(Timestamp arrivalDate) {
		this.arrivalDate = arrivalDate;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getMessageTypeCode() {
		return messageTypeCode;
	}

	public void setMessageTypeCode(String messageTypeCode) {
		this.messageTypeCode = messageTypeCode;
	}

	public Message getMessage() {
		return message;
	}

	public void setMessage(Message message) {
		this.message = message;
	}

	public String getUserMessage() {
		return userMessage;
	}

	public void setUserMessage(String userMessage) {
		this.userMessage = userMessage;
	}

	public String getToPersonId() {
		return toPersonId;
	}

	public void setToPersonId(String toPersonId) {
		this.toPersonId = toPersonId;
	}

	public Module getModuleName() {
		return moduleName;
	}

	public void setModuleName(Module moduleName) {
		this.moduleName = moduleName;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public String getOpenedFlag() {
		return openedFlag;
	}

	public void setOpenedFlag(String openedFlag) {
		this.openedFlag = openedFlag;
	}

	public String getAlertType() {
		return alertType;
	}

	public void setAlertType(String alertType) {
		this.alertType = alertType;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
	}

}

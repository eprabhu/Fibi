package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_LOG")
public class ManpowerLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "MANPOWER_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_MANPOWER_LOG")
	@SequenceGenerator(name="SEQ_MANPOWER_LOG", sequenceName = "SEQ_MANPOWER_LOG", allocationSize=1)
	private Integer manpowerLogId;

	@Column(name = "WORKDAY_MANPOWER_INTERFACE_ID")
	private Integer workdayManpowerIntefaceId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "REQUEST_PARAM")
	private String requestParam;

	@Column(name = "MESSAGE")
	private String message;

	@Column(name = "MESSAGE_TYPE")
	private String messageType;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@Column(name = "INTERFACE_TYPE_CODE")
	private String interfaceTypeCode;

	@Column(name = "IS_MAIL_SENT")
	private String isMailSent;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ERROR_DETAIL_MESSAGE")
	private String errorDetailmessage;

	@Column(name = "ERROR_XPATH")
	private String errorXPath;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	private transient String awardTitle;

	private transient String manpowerInterfaceType;

	public Integer getManpowerLogId() {
		return manpowerLogId;
	}

	public void setManpowerLogId(Integer manpowerLogId) {
		this.manpowerLogId = manpowerLogId;
	}

	public Integer getWorkdayManpowerIntefaceId() {
		return workdayManpowerIntefaceId;
	}

	public void setWorkdayManpowerIntefaceId(Integer workdayManpowerIntefaceId) {
		this.workdayManpowerIntefaceId = workdayManpowerIntefaceId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getRequestParam() {
		return requestParam;
	}

	public void setRequestParam(String requestParam) {
		this.requestParam = requestParam;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getMessageType() {
		return messageType;
	}

	public void setMessageType(String messageType) {
		this.messageType = messageType;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public String getInterfaceTypeCode() {
		return interfaceTypeCode;
	}

	public void setInterfaceTypeCode(String interfaceTypeCode) {
		this.interfaceTypeCode = interfaceTypeCode;
	}

	public String getIsMailSent() {
		return isMailSent;
	}

	public void setIsMailSent(String isMailSent) {
		this.isMailSent = isMailSent;
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

	public String getErrorDetailmessage() {
		return errorDetailmessage;
	}

	public void setErrorDetailmessage(String errorDetailmessage) {
		this.errorDetailmessage = errorDetailmessage;
	}

	public String getErrorXPath() {
		return errorXPath;
	}

	public void setErrorXPath(String errorXPath) {
		this.errorXPath = errorXPath;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardTitle() {
		return awardTitle;
	}

	public void setAwardTitle(String awardTitle) {
		this.awardTitle = awardTitle;
	}

	public String getManpowerInterfaceType() {
		return manpowerInterfaceType;
	}

	public void setManpowerInterfaceType(String manpowerInterfaceType) {
		this.manpowerInterfaceType = manpowerInterfaceType;
	}

}

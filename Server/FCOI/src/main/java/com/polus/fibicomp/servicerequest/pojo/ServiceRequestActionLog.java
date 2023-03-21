package com.polus.fibicomp.servicerequest.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SR_ACTION_LOG")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestActionLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTION_LOG_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer actionLogId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_TYPE_CODE")
	private Integer actionTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_ACTION_LOG_FK2"), name = "ACTION_TYPE_CODE", referencedColumnName = "ACTION_TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestActionType serviceRequestActionType;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_ACTION_LOG_FK3"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ServiceRequestStatus serviceRequestStatus;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String assigneePersonId;

	@Column(name = "ASSIGNEE_PERSON_NAME")
	private String assigneePersonName;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Transient
	private String updateUserFullName;

	@Transient
	private ServiceRequestHistory serviceRequestHistory;

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public Integer getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(Integer actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public String getAssigneePersonName() {
		return assigneePersonName;
	}

	public void setAssigneePersonName(String assigneePersonName) {
		this.assigneePersonName = assigneePersonName;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public ServiceRequestStatus getServiceRequestStatus() {
		return serviceRequestStatus;
	}

	public void setServiceRequestStatus(ServiceRequestStatus serviceRequestStatus) {
		this.serviceRequestStatus = serviceRequestStatus;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public ServiceRequestActionType getServiceRequestActionType() {
		return serviceRequestActionType;
	}

	public void setServiceRequestActionType(ServiceRequestActionType serviceRequestActionType) {
		this.serviceRequestActionType = serviceRequestActionType;
	}

	@Transient
	private String message;

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public ServiceRequestHistory getServiceRequestHistory() {
		return serviceRequestHistory;
	}

	public void setServiceRequestHistory(ServiceRequestHistory serviceRequestHistory) {
		this.serviceRequestHistory = serviceRequestHistory;
	}

}

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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.agreements.pojo.AdminGroup;

@Entity
@Table(name = "SR_STATUS_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestStatusHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SR_STATUS_HISTORY_ID", length = 8)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer statusHistoryId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_STATUS_HISTORY_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ServiceRequestStatus serviceRequestStatus;

	@Column(name = "ACTION_START_TIME")
	private Timestamp actionStartTime;

	@Column(name = "ACTION_END_TIME")
	private Timestamp actionEndTime;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_STATUS_HISTORY_FK4"), name = "ADMIN_GROUP_ID", referencedColumnName = "ADMIN_GROUP_ID", insertable = false, updatable = false)
	private AdminGroup adminGroup;

	public Integer getStatusHistoryId() {
		return statusHistoryId;
	}

	public void setStatusHistoryId(Integer statusHistoryId) {
		this.statusHistoryId = statusHistoryId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getActionStartTime() {
		return actionStartTime;
	}

	public void setActionStartTime(Timestamp actionStartTime) {
		this.actionStartTime = actionStartTime;
	}

	public Timestamp getActionEndTime() {
		return actionEndTime;
	}

	public void setActionEndTime(Timestamp actionEndTime) {
		this.actionEndTime = actionEndTime;
	}

	public ServiceRequestStatus getServiceRequestStatus() {
		return serviceRequestStatus;
	}

	public void setServiceRequestStatus(ServiceRequestStatus serviceRequestStatus) {
		this.serviceRequestStatus = serviceRequestStatus;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public AdminGroup getAdminGroup() {
		return adminGroup;
	}

	public void setAdminGroup(AdminGroup adminGroup) {
		this.adminGroup = adminGroup;
	}

}

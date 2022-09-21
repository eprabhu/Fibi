package com.polus.fibicomp.servicerequest.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SR_PROCESS_FLOW")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestProcessFlow implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROCESS_FLOW_ID", length = 12)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer processFlowId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_PROCESS_FLOW_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ServiceRequestStatus serviceRequestStatus;

	@Column(name = "ROLE_TYPE_CODE", length = 3)
	private String roleTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_PROCESS_FLOW_FK3"), name = "ROLE_TYPE_CODE", referencedColumnName = "ROLE_TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestRoleType serviceRequestRoleType;

	@Column(name = "HAS_REVIEW_RIGHT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasReviewRight = false;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Column(name = "PROCESS_START_TIMESTAMP")
	private Timestamp processStartTimestamp;

	@Column(name = "PROCESS_END_TIMESTAMP")
	private Timestamp processEndTimestamp;

	public Integer getProcessFlowId() {
		return processFlowId;
	}

	public void setProcessFlowId(Integer processFlowId) {
		this.processFlowId = processFlowId;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public String getRoleTypeCode() {
		return roleTypeCode;
	}

	public void setRoleTypeCode(String roleTypeCode) {
		this.roleTypeCode = roleTypeCode;
	}

	public Boolean getHasReviewRight() {
		return hasReviewRight;
	}

	public void setHasReviewRight(Boolean hasReviewRight) {
		this.hasReviewRight = hasReviewRight;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getProcessStartTimestamp() {
		return processStartTimestamp;
	}

	public void setProcessStartTimestamp(Timestamp processStartTimestamp) {
		this.processStartTimestamp = processStartTimestamp;
	}

	public Timestamp getProcessEndTimestamp() {
		return processEndTimestamp;
	}

	public void setProcessEndTimestamp(Timestamp processEndTimestamp) {
		this.processEndTimestamp = processEndTimestamp;
	}

	public ServiceRequestStatus getServiceRequestStatus() {
		return serviceRequestStatus;
	}

	public void setServiceRequestStatus(ServiceRequestStatus serviceRequestStatus) {
		this.serviceRequestStatus = serviceRequestStatus;
	}

	public ServiceRequestRoleType getServiceRequestRoleType() {
		return serviceRequestRoleType;
	}

	public void setServiceRequestRoleType(ServiceRequestRoleType serviceRequestRoleType) {
		this.serviceRequestRoleType = serviceRequestRoleType;
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

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

}

package com.polus.fibicomp.servicerequest.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SR_REPORTER_CHANGE_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestReporterChangeHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REPORTER_CHANGE_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer reporterChangeHistoryId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "OLD_REPORTER_PERSON_ID")
	private String oldReporterPersonId;

	@Column(name = "NEW_REPORTER_PERSON_ID")
	private String newReporterPersonId;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;


	public Integer getReporterChangeHistoryId() {
		return reporterChangeHistoryId;
	}

	public void setReporterChangeHistoryId(Integer reporterChangeHistoryId) {
		this.reporterChangeHistoryId = reporterChangeHistoryId;
	}

	public String getOldReporterPersonId() {
		return oldReporterPersonId;
	}

	public void setOldReporterPersonId(String oldReporterPersonId) {
		this.oldReporterPersonId = oldReporterPersonId;
	}

	public String getNewReporterPersonId() {
		return newReporterPersonId;
	}

	public void setNewReporterPersonId(String newReporterPersonId) {
		this.newReporterPersonId = newReporterPersonId;
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

}

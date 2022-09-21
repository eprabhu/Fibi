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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SR_WATCHER")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestWatcher implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WATCHER_ID", length = 8)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer watcherId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "WATCHER_PERSON_ID")
	private String watcherPersonId;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Transient
	private String acType;

	@Transient
	private String watcherName;
	
	public Integer getWatcherId() {
		return watcherId;
	}

	public void setWatcherId(Integer watcherId) {
		this.watcherId = watcherId;
	}

	public String getWatcherPersonId() {
		return watcherPersonId;
	}

	public void setWatcherPersonId(String watcherPersonId) {
		this.watcherPersonId = watcherPersonId;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public String getWatcherName() {
		return watcherName;
	}

	public void setWatcherName(String watcherName) {
		this.watcherName = watcherName;
	}

}

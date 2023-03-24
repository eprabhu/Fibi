package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.task.pojo.TaskType;

@Entity
@Table(name = "AWARD_TASK_TYPE_MAPPING")
public class AwardTaskTypeMapping implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_TASK_TYPE_MAPPING_ID")
	private Integer awardTaskTypeMappingId;

	@Column(name = "AWARD_DOCUMENT_TYPE_CODE")
	private String awardDocumentTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_TASK_TYPE_MAPPING_FK1"), name = "AWARD_DOCUMENT_TYPE_CODE", referencedColumnName = "AWARD_DOCUMENT_TYPE_CODE", insertable = false, updatable = false)
	private AwardDocumentType awardDocumentType;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@Column(name = "TASK_TYPE_CODE")
	private String taskTypeCode;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_TASK_TYPE_MAPPING_FK3"), name = "TASK_TYPE_CODE", referencedColumnName = "TASK_TYPE_CODE", insertable = false, updatable = false)
	private TaskType taskType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private ServiceRequestType serviceRequestType;

	public Integer getAwardTaskTypeMappingId() {
		return awardTaskTypeMappingId;
	}

	public void setAwardTaskTypeMappingId(Integer awardTaskTypeMappingId) {
		this.awardTaskTypeMappingId = awardTaskTypeMappingId;
	}

	public String getAwardDocumentTypeCode() {
		return awardDocumentTypeCode;
	}

	public void setAwardDocumentTypeCode(String awardDocumentTypeCode) {
		this.awardDocumentTypeCode = awardDocumentTypeCode;
	}

	public AwardDocumentType getAwardDocumentType() {
		return awardDocumentType;
	}

	public void setAwardDocumentType(AwardDocumentType awardDocumentType) {
		this.awardDocumentType = awardDocumentType;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public String getTaskTypeCode() {
		return taskTypeCode;
	}

	public void setTaskTypeCode(String taskTypeCode) {
		this.taskTypeCode = taskTypeCode;
	}

	public TaskType getTaskType() {
		return taskType;
	}

	public void setTaskType(TaskType taskType) {
		this.taskType = taskType;
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

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

}

package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "COST_ELEMENT_JOB_PROFILE_MAPPING")
public class CostElementJobProfileMapping implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COST_JOB_MAPPING_ID")
	private String costJobMappingId;

	@Column(name = "COST_ELEMENT")
	private String costElementCode;

	@Column(name = "JOB_PROFILE_CODE")
	private String jobProfileCode;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getCostJobMappingId() {
		return costJobMappingId;
	}

	public void setCostJobMappingId(String costJobMappingId) {
		this.costJobMappingId = costJobMappingId;
	}

	public String getCostElementCode() {
		return costElementCode;
	}

	public void setCostElementCode(String costElementCode) {
		this.costElementCode = costElementCode;
	}

	public String getJobProfileCode() {
		return jobProfileCode;
	}

	public void setJobProfileCode(String jobProfileCode) {
		this.jobProfileCode = jobProfileCode;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}

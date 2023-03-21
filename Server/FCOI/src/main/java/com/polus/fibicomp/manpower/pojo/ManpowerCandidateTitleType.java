package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_CANDIDATE_TITLE_TYPE")
public class ManpowerCandidateTitleType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CANDIDATE_TITLE_TYPE_CODE")
	private String candidateTitleTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getCandidateTitleTypeCode() {
		return candidateTitleTypeCode;
	}

	public void setCandidateTitleTypeCode(String candidateTitleTypeCode) {
		this.candidateTitleTypeCode = candidateTitleTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
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

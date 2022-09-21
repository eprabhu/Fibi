package com.polus.fibicomp.externalreviewer.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "EXT_REVIEWER_ACADEMIC_RANK")
public class ExtReviewerAcademicRank implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACADEMIC_RANK_CODE")
	private String academicRankCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getAcademicRankCode() {
		return academicRankCode;
	}

	public void setAcademicRankCode(String academicRankCode) {
		this.academicRankCode = academicRankCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

}

package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "REVIEW_STATUS")
public class ReviewStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REVIEW_STATUS_CODE", length = 3)
	private Integer reviewStatusCode;

	@Column(name = "DESCRIPTION", length = 200)
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	public Integer getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(Integer reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
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

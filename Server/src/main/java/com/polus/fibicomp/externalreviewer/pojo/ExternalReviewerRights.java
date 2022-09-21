package com.polus.fibicomp.externalreviewer.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "EXTERNAL_REVIEWER_RIGHTS")
public class ExternalReviewerRights implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ROLES_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer personRoleId;
	
	@Column(name = "REVIEWER_RIGHTS_ID")
	private String reviewerRightId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_RIGHTS_FK1"), name = "REVIEWER_RIGHTS_ID", referencedColumnName = "REVIEWER_RIGHTS_ID", insertable = false, updatable = false)
	private ReviewerRights reviewerRights;
	
	@Column(name = "EXTERNAL_REVIEWER_ID")
	private Integer externalReviewerId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_RIGHTS_FK2"), name = "EXTERNAL_REVIEWER_ID", referencedColumnName = "EXTERNAL_REVIEWER_ID", insertable = false, updatable = false)
	private ExternalReviewer externalReviewer;

	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public String getReviewerRightId() {
		return reviewerRightId;
	}

	public void setReviewerRightId(String reviewerRightId) {
		this.reviewerRightId = reviewerRightId;
	}

	public ReviewerRights getReviewerRights() {
		return reviewerRights;
	}

	public void setReviewerRights(ReviewerRights reviewerRights) {
		this.reviewerRights = reviewerRights;
	}

	public Integer getExternalReviewerId() {
		return externalReviewerId;
	}

	public void setExternalReviewerId(Integer externalReviewerId) {
		this.externalReviewerId = externalReviewerId;
	}

	public ExternalReviewer getExternalReviewer() {
		return externalReviewer;
	}

	public void setExternalReviewer(ExternalReviewer externalReviewer) {
		this.externalReviewer = externalReviewer;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
	
}
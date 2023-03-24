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
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "EXTERNAL_REVIEWER_SPECIALIZATION")
public class ExternalReviewerSpecialization implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EXTERNAL_REVIEWER_SPECIALIZATION_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer extReviewerSpecializationId;
	
	@Column(name = "SPECIALIZATION_CODE")
	private String specializationCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_SPECIALIZATION_FK1"), name = "SPECIALIZATION_CODE", referencedColumnName = "SPECIALISM_KEYWORD_CODE", insertable = false, updatable = false)
	private SpecialismKeyword extReviewerSpecialization;

	@Column(name = "EXTERNAL_REVIEWER_ID")
	private Integer externalReviewerId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_SPECIALIZATION_FK2"), name = "EXTERNAL_REVIEWER_ID", referencedColumnName = "EXTERNAL_REVIEWER_ID", insertable = false, updatable = false)
	private ExternalReviewer externalReviewer;

	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String actionType ;

	public Integer getExtReviewerSpecializationId() {
		return extReviewerSpecializationId;
	}

	public void setExtReviewerSpecializationId(Integer extReviewerSpecializationId) {
		this.extReviewerSpecializationId = extReviewerSpecializationId;
	}

	public SpecialismKeyword getExtReviewerSpecialization() {
		return extReviewerSpecialization;
	}

	public void setExtReviewerSpecialization(SpecialismKeyword extReviewerSpecialization) {
		this.extReviewerSpecialization = extReviewerSpecialization;
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

	public String getSpecializationCode() {
		return specializationCode;
	}

	public void setSpecializationCode(String specializationCode) {
		this.specializationCode = specializationCode;
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

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

}

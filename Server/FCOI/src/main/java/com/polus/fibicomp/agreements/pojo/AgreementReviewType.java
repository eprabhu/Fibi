package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AGREEMENT_REVIEW_TYPE")
public class AgreementReviewType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGREEMENT_REVIEW_TYPE_CODE")
	private String agreementReviewTypeCode;

	@Column(name = "LOCATION_TYPE_CODE")
	private String locationTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_REVIEW_TYPE_FK1"), name = "LOCATION_TYPE_CODE", referencedColumnName = "LOCATION_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsLocationType negotiationsLocationType;

	@Column(name = "WORKFLOW_STATUS_CODE")
	private String workflowStatusCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_REVIEW_TYPE_FK2"), name = "WORKFLOW_STATUS_CODE", referencedColumnName = "WORKFLOW_STATUS_CODE", insertable = false, updatable = false)
	private AgreementWorkflowStatus agreementWorkflowStatus;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive = false;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getLocationTypeCode() {
		return locationTypeCode;
	}

	public void setLocationTypeCode(String locationTypeCode) {
		this.locationTypeCode = locationTypeCode;
	}

	public NegotiationsLocationType getNegotiationsLocationType() {
		return negotiationsLocationType;
	}

	public void setNegotiationsLocationType(NegotiationsLocationType negotiationsLocationType) {
		this.negotiationsLocationType = negotiationsLocationType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

	public String getWorkflowStatusCode() {
		return workflowStatusCode;
	}

	public void setWorkflowStatusCode(String workflowStatusCode) {
		this.workflowStatusCode = workflowStatusCode;
	}

	public AgreementWorkflowStatus getAgreementWorkflowStatus() {
		return agreementWorkflowStatus;
	}

	public void setAgreementWorkflowStatus(AgreementWorkflowStatus agreementWorkflowStatus) {
		this.agreementWorkflowStatus = agreementWorkflowStatus;
	}

	public String getAgreementReviewTypeCode() {
		return agreementReviewTypeCode;
	}

	public void setAgreementReviewTypeCode(String agreementReviewTypeCode) {
		this.agreementReviewTypeCode = agreementReviewTypeCode;
	}

}

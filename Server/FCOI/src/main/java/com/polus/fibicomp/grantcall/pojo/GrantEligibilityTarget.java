package com.polus.fibicomp.grantcall.pojo;

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
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "GRANT_ELIGIBILITY_TARGET")
public class GrantEligibilityTarget implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_ELIGIBILITY_TARGET_ID_GENERATOR")
	@SequenceGenerator(name = "GRANT_ELIGIBILITY_TARGET_ID_GENERATOR", sequenceName = "GRANT_ELIGIBILITY_TARGET_ID_GENERATOR", allocationSize = 1)
	@Column(name = "GRANT_ELIGIBILITY_TARGET_ID")
	private Integer grantEligibilityTargetId;

	@JsonBackReference
	@OneToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_ELIGIBILITY_TARGET_FK"), name = "GRANT_ELIGIBILITY_ID", referencedColumnName = "GRANT_ELIGIBILITY_ID")
	private GrantCallEligibility grantCallEligibility;

	@Column(name = "ELIGIBILITY_TARGET_TYPE_CODE")
	private String eligibilityTargetTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_ELIGIBILITY_TARGET_FK2"), name = "ELIGIBILITY_TARGET_TYPE_CODE", referencedColumnName = "ELIGIBILITY_TARGET_TYPE_CODE", insertable = false, updatable = false)
	private GrantEligibiltyTargetType grantEligibiltyTargetType;

	@Column(name = "TARGET_CATEGORY_TYPE_CODE")
	private String targetCategoryTypeCode;

	@Column(name = "TARGET_VALUE")
	private String targetValue;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp; 

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String targetCategoryType;

	@Transient
	private String targetValueDescription;

	public Integer getGrantEligibilityTargetId() {
		return grantEligibilityTargetId;
	}

	public void setGrantEligibilityTargetId(Integer grantEligibilityTargetId) {
		this.grantEligibilityTargetId = grantEligibilityTargetId;
	}

	public GrantCallEligibility getGrantCallEligibility() {
		return grantCallEligibility;
	}

	public void setGrantCallEligibility(GrantCallEligibility grantCallEligibility) {
		this.grantCallEligibility = grantCallEligibility;
	}

	public String getEligibilityTargetTypeCode() {
		return eligibilityTargetTypeCode;
	}

	public void setEligibilityTargetTypeCode(String eligibilityTargetTypeCode) {
		this.eligibilityTargetTypeCode = eligibilityTargetTypeCode;
	}

	public String getTargetValue() {
		return targetValue;
	}

	public void setTargetValue(String targetValue) {
		this.targetValue = targetValue;
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

	public String getTargetCategoryTypeCode() {
		return targetCategoryTypeCode;
	}

	public void setTargetCategoryTypeCode(String targetCategoryTypeCode) {
		this.targetCategoryTypeCode = targetCategoryTypeCode;
	}

	public String getTargetCategoryType() {
		return targetCategoryType;
	}

	public void setTargetCategoryType(String targetCategoryType) {
		this.targetCategoryType = targetCategoryType;
	}

	public String getTargetValueDescription() {
		return targetValueDescription;
	}

	public void setTargetValueDescription(String targetValueDescription) {
		this.targetValueDescription = targetValueDescription;
	}

	public GrantEligibiltyTargetType getGrantEligibiltyTargetType() {
		return grantEligibiltyTargetType;
	}

	public void setGrantEligibiltyTargetType(GrantEligibiltyTargetType grantEligibiltyTargetType) {
		this.grantEligibiltyTargetType = grantEligibiltyTargetType;
	}

}

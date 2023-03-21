package com.polus.fibicomp.claims.pojo;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.*;

@Entity
@Table(name = "CLAIM_FUNDING_SCHEME")
public class ClaimFundingScheme implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUNDING_SCHEME_CODE")
	private String fundingSchemeCode;
	
	@Column(name = "CERTIFICATION1")
	private String certification1;
	
	@Column(name = "CERTIFICATION2")
	private String certification2;
	
	@Column(name = "ENDORSEMENT")
	private String endorsement;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Id
	@Column(name = "LETTER_TEMPLATE_TYPE_CODE")
	private String letterTemplateTypeCode;

	@Column(name = "OVERRIDE_NEGATIVE_AMOUNT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean overrideNegativeAmount;

	public Boolean getOverrideNegativeAmount() {
		return overrideNegativeAmount;
	}

	public void setOverrideNegativeAmount(Boolean overrideNegativeAmount) {
		this.overrideNegativeAmount = overrideNegativeAmount;
	}

	public String getFundingSchemeCode() {
		return fundingSchemeCode;
	}

	public void setFundingSchemeCode(String fundingSchemeCode) {
		this.fundingSchemeCode = fundingSchemeCode;
	}

	public String getCertification1() {
		return certification1;
	}

	public void setCertification1(String certification1) {
		this.certification1 = certification1;
	}

	public String getCertification2() {
		return certification2;
	}

	public void setCertification2(String certification2) {
		this.certification2 = certification2;
	}

	public String getEndorsement() {
		return endorsement;
	}

	public void setEndorsement(String endorsement) {
		this.endorsement = endorsement;
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

	public String getLetterTemplateTypeCode() {
		return letterTemplateTypeCode;
	}

	public void setLetterTemplateTypeCode(String letterTemplateTypeCode) {
		this.letterTemplateTypeCode = letterTemplateTypeCode;
	}
}

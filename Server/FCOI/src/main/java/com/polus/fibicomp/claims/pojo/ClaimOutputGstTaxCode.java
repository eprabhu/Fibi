package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_OUTPUT_GST_TAX_CODE")
public class ClaimOutputGstTaxCode implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "OUTPUT_GST_CATEGORY")
	private String outputGstCategory;
	
	@Column(name = "TAX_CODE")
	private String taxCode;
	
	@Column(name = "TAX_DESCRIPTION")
	private String taxDescription;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;
	
	@Column(name ="UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getOutputGstCategory() {
		return outputGstCategory;
	}

	public void setOutputGstCategory(String outputGstCategory) {
		this.outputGstCategory = outputGstCategory;
	}

	public String getTaxCode() {
		return taxCode;
	}

	public void setTaxCode(String taxCode) {
		this.taxCode = taxCode;
	}

	public String getTaxDescription() {
		return taxDescription;
	}

	public void setTaxDescription(String taxDescription) {
		this.taxDescription = taxDescription;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

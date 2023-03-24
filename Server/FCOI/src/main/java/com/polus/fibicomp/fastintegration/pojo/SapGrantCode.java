package com.polus.fibicomp.fastintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SAP_GRANT_CODE")
public class SapGrantCode implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Column(name = "GRANT_TYPE")
	private String grantType;

	@Column(name = "GRANT_CODE_NAME")
	private String grantCodeName;

	@Column(name = "VALID_FROM")
	private Timestamp validFrom;

	@Column(name = "VALID_TO")
	private Timestamp validTo;

	@Column(name = "GRANT_CURRENCY")
	private String grantCurrency;

	@Column(name = "REMARKS")
	private String remarks;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "CAMPUS")
	private String campus;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "GM_SPONSOR")
	private String gmSponsor;
	
	@Column(name = "DATE_WHEN_FEED_INACTIVE")
	private Timestamp dateWhenFeedInactive;

	@Transient
	private String grantDetails;

	public Timestamp getDateWhenFeedInactive() {
		return dateWhenFeedInactive;
	}

	public void setDateWhenFeedInactive(Timestamp dateWhenFeedInactive) {
		this.dateWhenFeedInactive = dateWhenFeedInactive;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getGmSponsor() {
		return gmSponsor;
	}

	public void setGmSponsor(String gmSponsor) {
		this.gmSponsor = gmSponsor;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getGrantType() {
		return grantType;
	}

	public void setGrantType(String grantType) {
		this.grantType = grantType;
	}

	public String getGrantCodeName() {
		return grantCodeName;
	}

	public void setGrantCodeName(String grantCodeName) {
		this.grantCodeName = grantCodeName;
	}

	public Timestamp getValidFrom() {
		return validFrom;
	}

	public void setValidFrom(Timestamp validFrom) {
		this.validFrom = validFrom;
	}

	public Timestamp getValidTo() {
		return validTo;
	}

	public void setValidTo(Timestamp validTo) {
		this.validTo = validTo;
	}

	public String getGrantCurrency() {
		return grantCurrency;
	}

	public void setGrantCurrency(String grantCurrency) {
		this.grantCurrency = grantCurrency;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public String getCampus() {
		return campus;
	}

	public void setCampus(String campus) {
		this.campus = campus;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
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

	public String getGrantDetails() {
		StringBuilder grantName = new StringBuilder(grantCode);
		grantDetails = grantName.append(" - ").append(grantCodeName).toString();
		return grantDetails;
	}

	public void setGrantDetails(String grantDetails) {
		this.grantDetails = grantDetails;
	}
}

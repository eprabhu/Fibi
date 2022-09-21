package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "SPONSOR_FUNDING_SCHEME")
public class SponsorFundingScheme implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUNDING_SCHEME_ID")
	private Integer fundingSchemeId;

	@Column(name = "FUNDING_SCHEME_CODE")
	private String fundingSchemeCode;

	@ManyToOne
   	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_FUNDING_SCHEME_FK1"), name = "FUNDING_SCHEME_CODE", referencedColumnName = "FUNDING_SCHEME_CODE", insertable = false, updatable = false)
   	private FundingScheme fundingScheme;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne
   	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_FUNDING_SCHEME_FK2"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
   	private Sponsor sponsor;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public SponsorFundingScheme(Integer fundingSchemeId, String description) {
		super();
		this.fundingSchemeId = fundingSchemeId;
		this.description = description;
	}

	public SponsorFundingScheme() {
		super();
	}

	public Integer getFundingSchemeId() {
		return fundingSchemeId;
	}

	public void setFundingSchemeId(Integer fundingSchemeId) {
		this.fundingSchemeId = fundingSchemeId;
	}

	public String getFundingSchemeCode() {
		return fundingSchemeCode;
	}

	public void setFundingSchemeCode(String fundingSchemeCode) {
		this.fundingSchemeCode = fundingSchemeCode;
	}

	public FundingScheme getFundingScheme() {
		return fundingScheme;
	}

	public void setFundingScheme(FundingScheme fundingScheme) {
		this.fundingScheme = fundingScheme;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
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

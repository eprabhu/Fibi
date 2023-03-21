package com.polus.fibicomp.pojo;


import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "FUNDING_SOURCE_TYPE")
public class FundingSourceType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
    @Column(name = "FUNDING_SOURCE_TYPE_CODE", updatable = false, nullable = false)
	private String fundingSourceTypeCode;

    @Column(name = "DESCRIPTION")
	private String description;

    @Column(name = "FUNDING_SOURCE_TYPE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean fundingSourceTypeFlag;
   
    @Column(name = "SPONSOR_CODE")
   	private String sponsorCode;

   	@ManyToOne(cascade = { CascadeType.REFRESH })
   	@JoinColumn(foreignKey = @ForeignKey(name = "FUNDING_SOURCE_TYPE_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
   	private Sponsor sponsor;

	public FundingSourceType() {
		
	}

	public FundingSourceType(String fundingSourceTypeCode, String description) {
		super();
		this.fundingSourceTypeCode = fundingSourceTypeCode;
		this.description = description;
	}

	public String getFundingSourceTypeCode() {
		return fundingSourceTypeCode;
	}

	public void setFundingSourceTypeCode(String fundingSourceTypeCode) {
		this.fundingSourceTypeCode = fundingSourceTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public boolean getFundingSourceTypeFlag() {
		return fundingSourceTypeFlag;
	}

	public void setFundingSourceTypeFlag(boolean fundingSourceTypeFlag) {
		this.fundingSourceTypeFlag = fundingSourceTypeFlag;
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

}

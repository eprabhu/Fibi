package com.polus.fibicomp.view;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Immutable;

@Entity
@Immutable
@Table(name = "CUSTOM_DATA_V")
public class CustomDataView implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;
	
	@Column(name = "GRANT_CODE")
	private String grantCode;
	
	@Column(name = "OUTPUT_GST_CATEGORY")
	private String outputGstCategory;
	
	@Column(name = "STEM_NONSTEM")
	private String stemNonStem;
	
	@Column(name = "RIE_DOMAIN")
	private String rieDomain;
	
	@Column(name = "SUB_LEAD_UNIT")
	private String subLeadUnit;
	
	@Column(name = "PROFIT_CENTER")
	private String profitCenter;
	
	@Column(name = "FUND_CENTER")
	private String fundCenter;
	
	@Column(name = "COST_CENTER")
	private String costCenter;
	
	@Column(name = "CLAIM_PREPARER")
	private String claimPreparer;

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getOutputGstCategory() {
		return outputGstCategory;
	}

	public void setOutputGstCategory(String outputGstCategory) {
		this.outputGstCategory = outputGstCategory;
	}

	public String getStemNonStem() {
		return stemNonStem;
	}

	public void setStemNonStem(String stemNonStem) {
		this.stemNonStem = stemNonStem;
	}

	public String getRieDomain() {
		return rieDomain;
	}

	public void setRieDomain(String rieDomain) {
		this.rieDomain = rieDomain;
	}

	public String getSubLeadUnit() {
		return subLeadUnit;
	}

	public void setSubLeadUnit(String subLeadUnit) {
		this.subLeadUnit = subLeadUnit;
	}

	public String getProfitCenter() {
		return profitCenter;
	}

	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public String getCostCenter() {
		return costCenter;
	}

	public void setCostCenter(String costCenter) {
		this.costCenter = costCenter;
	}

	public String getClaimPreparer() {
		return claimPreparer;
	}

	public void setClaimPreparer(String claimPreparer) {
		this.claimPreparer = claimPreparer;
	}	
}

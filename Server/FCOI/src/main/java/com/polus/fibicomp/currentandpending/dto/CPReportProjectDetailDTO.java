package com.polus.fibicomp.currentandpending.dto;

public class CPReportProjectDetailDTO {

	private String totalAwardAmount;

	private String annualDirectCost;

	private String percentageOfEffort;

	private String projectGoals;

	private String specificAims;

	private String projectSummary; 

	private String comments;

	private String rolePlayed;

	private String grantCallName;

	private String fundingType;

	private String fundingSource;

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public String getFundingType() {
		return fundingType;
	}

	public void setFundingType(String fundingType) {
		this.fundingType = fundingType;
	}

	public String getFundingSource() {
		return fundingSource;
	}

	public void setFundingSource(String fundingSource) {
		this.fundingSource = fundingSource;
	}

	public String getProjectGoals() {
		return projectGoals;
	}

	public void setProjectGoals(String projectGoals) {
		this.projectGoals = projectGoals;
	}

	public String getSpecificAims() {
		return specificAims;
	}

	public void setSpecificAims(String specificAims) {
		this.specificAims = specificAims;
	}

	public String getProjectSummary() {
		return projectSummary;
	}

	public void setProjectSummary(String projectSummary) {
		this.projectSummary = projectSummary;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getTotalAwardAmount() {
		return totalAwardAmount;
	}

	public void setTotalAwardAmount(String totalAwardAmount) {
		this.totalAwardAmount = totalAwardAmount;
	}

	public String getAnnualDirectCost() {
		return annualDirectCost;
	}

	public void setAnnualDirectCost(String annualDirectCost) {
		this.annualDirectCost = annualDirectCost;
	}

	public String getPercentageOfEffort() {
		return percentageOfEffort;
	}

	public void setPercentageOfEffort(String percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
	}

	public String getRolePlayed() {
		return rolePlayed;
	}

	public void setRolePlayed(String rolePlayed) {
		this.rolePlayed = rolePlayed;
	}

}

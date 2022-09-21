package com.polus.fibicomp.awardbudgetapiintegration.vo;

public class AwardBudgetPrintVO {

	private Integer awardId;
	
	private String projectNumber;
	
	private Integer year; 
	
	private String exportType;
	
	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getProjectNumber() {
		return projectNumber;
	}

	public void setProjectNumber(String projectNumber) {
		this.projectNumber = projectNumber;
	}

	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public String getExportType() {
		return exportType;
	}

	public void setExportType(String exportType) {
		this.exportType = exportType;
	}

}

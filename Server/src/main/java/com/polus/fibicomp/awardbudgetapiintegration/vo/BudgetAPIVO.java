package com.polus.fibicomp.awardbudgetapiintegration.vo;

import java.util.List;

import com.polus.fibicomp.awardbudgetapiintegration.pojo.BudgetAPIResponse;

public class BudgetAPIVO {

	private String year;

	private String projectNumber;

	private String taskNumber;

	private Integer projectId;
	
	private String projectName;

	private String longName;

	private List<BudgetAPIResponse> budgetAPIResponses;

	public List<BudgetAPIResponse> getBudgetAPIResponses() {
		return budgetAPIResponses;
	}

	public void setBudgetAPIResponses(List<BudgetAPIResponse> budgetAPIResponses) {
		this.budgetAPIResponses = budgetAPIResponses;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getProjectNumber() {
		return projectNumber;
	}

	public void setProjectNumber(String projectNumber) {
		this.projectNumber = projectNumber;
	}

	public String getTaskNumber() {
		return taskNumber;
	}

	public void setTaskNumber(String taskNumber) {
		this.taskNumber = taskNumber;
	}

	public Integer getProjectId() {
		return projectId;
	}

	public void setProjectId(Integer projectId) {
		this.projectId = projectId;
	}

	public String getProjectName() {
		return projectName;
	}

	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	public String getLongName() {
		return longName;
	}

	public void setLongName(String longName) {
		this.longName = longName;
	}
}

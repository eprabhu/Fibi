package com.polus.fibicomp.servicerequest.dto;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.pojo.ProposalPersonRole;

public class ProjectDetail {

	private String projectNumber;

	private Integer projectId;

	private String leadUnitNumber;

	private Timestamp startDate;

	private Timestamp endDate;

	private String unitName;

	private String piName;

	private String title;

	private Integer budgetTotalCost;

	private Timestamp budgetStartDate;

	private Timestamp budgetEndDate;

	private Integer budgetVersionNumber;

	private String description;

	private Integer serviceRequestProjectId;

	private List<ProposalPersonRole> researchMemberRoles;

	public String getProjectNumber() {
		return projectNumber;
	}

	public void setProjectNumber(String projectNumber) {
		this.projectNumber = projectNumber;
	}

	public Integer getProjectId() {
		return projectId;
	}

	public void setProjectId(Integer projectId) {
		this.projectId = projectId;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Integer getBudgetTotalCost() {
		return budgetTotalCost;
	}

	public void setBudgetTotalCost(Integer budgetTotalCost) {
		this.budgetTotalCost = budgetTotalCost;
	}

	public Timestamp getBudgetStartDate() {
		return budgetStartDate;
	}

	public void setBudgetStartDate(Timestamp budgetStartDate) {
		this.budgetStartDate = budgetStartDate;
	}

	public Timestamp getBudgetEndDate() {
		return budgetEndDate;
	}

	public void setBudgetEndDate(Timestamp budgetEndDate) {
		this.budgetEndDate = budgetEndDate;
	}

	public Integer getBudgetVersionNumber() {
		return budgetVersionNumber;
	}

	public void setBudgetVersionNumber(Integer budgetVersionNumber) {
		this.budgetVersionNumber = budgetVersionNumber;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public List<ProposalPersonRole> getResearchMemberRoles() {
		return researchMemberRoles;
	}

	public void setResearchMemberRoles(List<ProposalPersonRole> researchMemberRoles) {
		this.researchMemberRoles = researchMemberRoles;
	}

	public Integer getServiceRequestProjectId() {
		return serviceRequestProjectId;
	}

	public void setServiceRequestProjectId(Integer serviceRequestProjectId) {
		this.serviceRequestProjectId = serviceRequestProjectId;
	}

}

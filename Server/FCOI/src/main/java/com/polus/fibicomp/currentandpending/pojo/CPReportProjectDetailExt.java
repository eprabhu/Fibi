package com.polus.fibicomp.currentandpending.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CP_REPORT_PROJECT_DETAILS_EXT")
public class CPReportProjectDetailExt  implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CP_REPORT_PROJECT_DETAIL_ID")
	private Integer cpReportProjectDetailId;

	@Column(name = "ACADEMIC_MONTHS")
	private String academicMonths;

	@Column(name = "CALENDAR_MONTHS")
	private String calenderMonths;

	@Column(name = "SUMMER_MONTHS")
	private String summerMonths;

	@Column(name = "TOTAL_AWARD_AMOUNT")
	private BigDecimal totalAwardAmount;

	@Column(name = "ANNUAL_DIRECT_COST")
	private BigDecimal annualDirectCost;

	@Column(name = "LEAD_PI_NON_EMPLOYEE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean leadPiNonEmployeeFlag;

	@Column(name = "LEAD_PI_PERSON_ID")
	private String leadPIPersonId;

	@Column(name = "LOCATION")
	private String location;

	@Column(name = "PERSON_ROLE_ID")
	private Integer personRoleId;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageOfEffort;

	@Column(name = "PRIME_AWARD")
	private String primeAward;

	@Column(name = "ACADEMIC_CONTACT_PERSON_ID")
	private String academicContactPersonId;

	@Column(name = "TECHNICAL_CONTACT_PERSON_ID")
	private String technicalContactPersonId;

	@Column(name = "PROJECT_REL_TO_PROPOSED_EFFORT")
	private String projectRelToProposedEffort;

	@Column(name = "PROJECT_GOALS")
	private String projectGoals;

	@Column(name = "SPECIFIC_AIMS")
	private String specificAims;

	@Column(name = "PROJECT_SUMMARY")
	private String projectSummary; 

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Transient
	private ProposalPersonRole proposalPersonRole;

	@Transient
	private String leadPrincipalInvestigator;

	public Integer getCpReportProjectDetailId() {
		return cpReportProjectDetailId;
	}

	public void setCpReportProjectDetailId(Integer cpReportProjectDetailId) {
		this.cpReportProjectDetailId = cpReportProjectDetailId;
	}

	public String getAcademicMonths() {
		return academicMonths;
	}

	public void setAcademicMonths(String academicMonths) {
		this.academicMonths = academicMonths;
	}

	public String getCalenderMonths() {
		return calenderMonths;
	}

	public void setCalenderMonths(String calenderMonths) {
		this.calenderMonths = calenderMonths;
	}

	public String getSummerMonths() {
		return summerMonths;
	}

	public void setSummerMonths(String summerMonths) {
		this.summerMonths = summerMonths;
	}

	public BigDecimal getTotalAwardAmount() {
		return totalAwardAmount;
	}

	public void setTotalAwardAmount(BigDecimal totalAwardAmount) {
		this.totalAwardAmount = totalAwardAmount;
	}

	public BigDecimal getAnnualDirectCost() {
		return annualDirectCost;
	}

	public void setAnnualDirectCost(BigDecimal annualDirectCost) {
		this.annualDirectCost = annualDirectCost;
	}

	public Boolean getLeadPiNonEmployeeFlag() {
		return leadPiNonEmployeeFlag;
	}

	public void setLeadPiNonEmployeeFlag(Boolean leadPiNonEmployeeFlag) {
		this.leadPiNonEmployeeFlag = leadPiNonEmployeeFlag;
	}

	public String getLeadPIPersonId() {
		return leadPIPersonId;
	}

	public void setLeadPIPersonId(String leadPIPersonId) {
		this.leadPIPersonId = leadPIPersonId;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public BigDecimal getPercentageOfEffort() {
		return percentageOfEffort;
	}

	public void setPercentageOfEffort(BigDecimal percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
	}

	public String getPrimeAward() {
		return primeAward;
	}

	public void setPrimeAward(String primeAward) {
		this.primeAward = primeAward;
	}

	public String getAcademicContactPersonId() {
		return academicContactPersonId;
	}

	public void setAcademicContactPersonId(String academicContactPersonId) {
		this.academicContactPersonId = academicContactPersonId;
	}

	public String getTechnicalContactPersonId() {
		return technicalContactPersonId;
	}

	public void setTechnicalContactPersonId(String technicalContactPersonId) {
		this.technicalContactPersonId = technicalContactPersonId;
	}

	public String getProjectRelToProposedEffort() {
		return projectRelToProposedEffort;
	}

	public void setProjectRelToProposedEffort(String projectRelToProposedEffort) {
		this.projectRelToProposedEffort = projectRelToProposedEffort;
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

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public String getLeadPrincipalInvestigator() {
		return leadPrincipalInvestigator;
	}

	public void setLeadPrincipalInvestigator(String leadPrincipalInvestigator) {
		this.leadPrincipalInvestigator = leadPrincipalInvestigator;
	}

}

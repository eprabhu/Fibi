package com.polus.fibicomp.proposal.pojo;

import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "EPS_PROPOSAL_PROJECT_TEAM")
public class ProposalProjectTeam {

	@Id
	@Column(name = "PROPOSAL_PROJECT_TEAM_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_PJCT_TEAM_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_PJCT_TEAM_ID_GENERATOR", sequenceName = "EPS_PROP_PJCT_TEAM_ID_GENERATOR", allocationSize=1)
	private Integer proposalProjectTeamId;

	@Transient
	private String acType;
	
	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@Column(name = "FULL_NAME")
	private String fullName;
	
	@Column(name = "PROJECT_ROLE")
	private String projectRole;
	
	@Column(name = "NON_EMPLOYEE_FLAG")
	private String nonEmployeeFlag;
	
	@Column(name = "PERCENTAGE_CHARGED", precision = 5, scale = 2)
	private BigDecimal percentageCharged;
	
	@Column(name = "START_DATE")
	private Timestamp startDate;
	
	@Column(name = "END_DATE")
	private Timestamp endDate;
	
	@Column(name = "IS_ACTIVE")
	private String isActive;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "DESIGNATION")
	private String designation;

	public Integer getProposalProjectTeamId() {
		return proposalProjectTeamId;
	}

	public void setProposalProjectTeamId(Integer proposalProjectTeamId) {
		this.proposalProjectTeamId = proposalProjectTeamId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getProjectRole() {
		return projectRole;
	}

	public void setProjectRole(String projectRole) {
		this.projectRole = projectRole;
	}

	public String getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(String nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
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

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public BigDecimal getPercentageCharged() {
		return percentageCharged;
	}

	public void setPercentageCharged(BigDecimal percentageCharged) {
		this.percentageCharged = percentageCharged;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}
	
}

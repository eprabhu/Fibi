package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROPOSAL_PROJECT_TEAM")
public class InstituteProposalProjectTeam implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "instProposalProjectTeamIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "instProposalProjectTeamIdGenerator")
	@Column(name = "PROPOSAL_PROJECT_TEAM_ID")
	private Integer proposalProjectTeamId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_PROJECT_TEAM_FK1"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID")
	private InstituteProposal instProposal;

	@Transient
	private String acType;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PROJECT_ROLE")
	private String projectRole;

	@Column(name = "NON_EMPLOYEE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean nonEmployeeFlag;

	@Column(name = "PERCENTAGE_CHARGED")
	private String percentageCharged;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getProposalProjectTeamId() {
		return proposalProjectTeamId;
	}

	public void setProposalProjectTeamId(Integer proposalProjectTeamId) {
		this.proposalProjectTeamId = proposalProjectTeamId;
	}

	public InstituteProposal getInstProposal() {
		return instProposal;
	}

	public void setInstProposal(InstituteProposal instProposal) {
		this.instProposal = instProposal;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
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

	public Boolean getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(Boolean nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
	}

	public String getPercentageCharged() {
		return percentageCharged;
	}

	public void setPercentageCharged(String percentageCharged) {
		this.percentageCharged = percentageCharged;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
}

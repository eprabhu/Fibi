package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROPOSAL_PERSONS")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_PERSON_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_PERSON_ID_GNTR")
	@SequenceGenerator(name="SEQ_IP_PERSON_ID_GNTR", sequenceName = "SEQ_IP_PERSON_ID_GNTR", allocationSize=1)
	private Integer proposalPersonId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PROP_PERSON_ROLE_ID")
	private Integer personRoleId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_PERSONS_FK2"), name = "PROP_PERSON_ROLE_ID", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole proposalPersonRole;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageOfEffort;

	@Column(name = "PROPOSAL_NUMBER")
	private String proposalNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@JsonManagedReference
	@OneToMany(mappedBy = "instProposalPerson", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<InstituteProposalPersonUnit> units;

	@JsonManagedReference
	@OneToMany(mappedBy = "instProposalPerson", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<InstituteProposalPersonAttachment> proposalPersonAttachment;

	@Column(name = "PI_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPi = false;

	@Column(name = "DESIGNATION")
	private String designation;

	@Column(name = "IS_MULTI_PI")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMultiPi = false;

	@Column(name = "DEPARTMENT")
	private String department;

  @Column(name = "PROJECT_ROLE")
	private String projectRole;
  
	@Transient
	private String emailAddress;


	public InstituteProposalPerson() {
		this.units = new ArrayList<>();
		this.proposalPersonAttachment = new ArrayList<>();
	}

	public Integer getProposalPersonId() {
		return proposalPersonId;
	}

	public void setProposalPersonId(Integer proposalPersonId) {
		this.proposalPersonId = proposalPersonId;
	}

//	public Proposal getProposal() {
//		return proposal;
//	}
//
//	public void setProposal(Proposal proposal) {
//		this.proposal = proposal;
//	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
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

	public BigDecimal getPercentageOfEffort() {
		return percentageOfEffort;
	}

	public void setPercentageOfEffort(BigDecimal percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public List<InstituteProposalPersonUnit> getUnits() {
		return units;
	}

	public void setUnits(List<InstituteProposalPersonUnit> units) {
		this.units = units;
	}

	public List<InstituteProposalPersonAttachment> getProposalPersonAttachment() {
		return proposalPersonAttachment;
	}

	public void setProposalPersonAttachment(List<InstituteProposalPersonAttachment> proposalPersonAttachment) {
		this.proposalPersonAttachment = proposalPersonAttachment;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsPi() {
		return isPi;
	}

	public void setIsPi(Boolean isPi) {
		this.isPi = isPi;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

	public Boolean getIsMultiPi() {
		return isMultiPi;
	}

	public void setIsMultiPi(Boolean isMultiPi) {
		this.isMultiPi = isMultiPi;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getProjectRole() {
		return projectRole;
	}

	public void setProjectRole(String projectRole) {
		this.projectRole = projectRole;
	}

//	public Integer getInstProposalId() {
//		return instProposalId;
//	}
//
//	public void setInstProposalId(Integer instProposalId) {
//		this.instProposalId = instProposalId;
//	}

}

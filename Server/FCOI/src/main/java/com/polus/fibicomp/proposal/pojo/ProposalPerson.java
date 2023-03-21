package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.proposal.comparator.ProposalPersonUnitComparator;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROPOSAL_PERSONS")
public class ProposalPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_PERSON_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_PERSON_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_PERSON_ID_GENERATOR", sequenceName = "EPS_PROP_PERSON_ID_GENERATOR", allocationSize=1)
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
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_PERSONS_FK2"), name = "PROP_PERSON_ROLE_ID", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole proposalPersonRole;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageOfEffort;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalPerson", orphanRemoval = true, cascade = { CascadeType.ALL, CascadeType.REMOVE })
	private List<ProposalPersonUnit> units;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalPerson", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<ProposalPersonAttachment> proposalPersonAttachment;

	@Column(name = "PI_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean isPi = false;

	@Column(name = "DESIGNATION")
	private String designation;

	@Column(name = "DEPARTMENT")
	private String department;

	@Column(name = "IS_MULTI_PI")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMultiPi = false;

	@Column(name = "IS_PERSON_CERTIFIED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean personCertified;

	@Column(name = "PROJECT_ROLE")
	private String projectRole;

	@Transient
	private Boolean isGenerated = false;

	@Transient
	private String primaryTitle;
	
	@Transient
	private String trainingStatus;
	
	@Transient
	private String emailAddress;

	public ProposalPerson() {
		units = new ArrayList<>();
		proposalPersonAttachment = new ArrayList<>();
	}

	public Integer getProposalPersonId() {
		return proposalPersonId;
	}

	public void setProposalPersonId(Integer proposalPersonId) {
		this.proposalPersonId = proposalPersonId;
	}

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

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public BigDecimal getPercentageOfEffort() {
		return percentageOfEffort;
	}

	public void setPercentageOfEffort(BigDecimal percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
	}

	public List<ProposalPersonUnit> getUnits() {
		if (units != null && !units.isEmpty()) {
			Collections.sort(units, new ProposalPersonUnitComparator());
		}
		return units;
	}

	public void setUnits(List<ProposalPersonUnit> units) {
		this.units = units;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public List<ProposalPersonAttachment> getProposalPersonAttachment() {
		return proposalPersonAttachment;
	}

	public void setProposalPersonAttachment(List<ProposalPersonAttachment> proposalPersonAttachment) {
		this.proposalPersonAttachment = proposalPersonAttachment;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
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

	public boolean getIsPi() {
		return isPi;
	}

	public void setIsPi(boolean isPi) {
		this.isPi = isPi;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public void setPi(boolean isPi) {
		this.isPi = isPi;
	}

	public Boolean getIsGenerated() {
		return isGenerated;
	}

	public void setIsGenerated(Boolean isGenerated) {
		this.isGenerated = isGenerated;
	}

	public String getPrimaryTitle() {
		return primaryTitle;
	}

	public void setPrimaryTitle(String primaryTitle) {
		this.primaryTitle = primaryTitle;
	}

	public Boolean getPersonCertified() {
		return personCertified;
	}

	public void setPersonCertified(Boolean personCertified) {
		this.personCertified = personCertified;
	}

	public String getProjectRole() {
		return projectRole;
	}

	public void setProjectRole(String projectRole) {
		this.projectRole = projectRole;
	}

	public String getTrainingStatus() {
		return trainingStatus;
	}

	public void setTrainingStatus(String trainingStatus) {
		this.trainingStatus = trainingStatus;
	}
}

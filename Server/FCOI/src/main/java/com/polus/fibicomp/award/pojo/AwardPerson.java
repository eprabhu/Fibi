package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.award.comparator.AwardPersonUnitComparator;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_PERSONS")
public class AwardPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_PERSON_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardPersonId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageEffort;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "DEPARTMENT")
	private String department;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "PERSON_ROLE_ID")
	private Integer personRoleId;
	
	@Column(name = "PROJECT_ROLE")
	private String projectRole;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSONS_FK2"), name = "PERSON_ROLE_ID", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole proposalPersonRole;

	@JsonManagedReference
	@OneToMany(mappedBy = "awardPerson", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<AwardPersonUnit> awardPersonUnits;

	@Column(name = "PI_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPi;

	@Column(name = "IS_MULTI_PI")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMultiPi = false;

	@JsonManagedReference
	@OneToMany(mappedBy = "awardPerson", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<AwardPersonAttachment> awardPersonAttachment;

	@Column(name = "DESIGNATION")
	private String designation;

	@Transient
	private String emailAddress;

	public AwardPerson(Integer awardId, String awardNumber, Integer sequenceNumber) {
		super();
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.sequenceNumber = sequenceNumber;
	}

	public AwardPerson(Integer awardPersonId, Integer awardId, String awardNumber, Integer sequenceNumber, String personId, String fullName,Integer rolodexId ) {
		super();
		this.awardPersonId = awardPersonId;
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.sequenceNumber = sequenceNumber;
		this.personId = personId;
		this.fullName = fullName;
		this.rolodexId = rolodexId;
	}

	public AwardPerson() {
		awardPersonUnits = new ArrayList<>();
		awardPersonAttachment = new ArrayList<>();
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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

	public BigDecimal getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(BigDecimal percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getProjectRole() {
		return projectRole;
	}

	public void setProjectRole(String projectRole) {
		this.projectRole = projectRole;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public List<AwardPersonUnit> getAwardPersonUnits() {
		if (awardPersonUnits != null && !awardPersonUnits.isEmpty()) {
			Collections.sort(awardPersonUnits, new AwardPersonUnitComparator());
		}
		return awardPersonUnits;
	}

	public void setAwardPersonUnits(List<AwardPersonUnit> awardPersonUnits) {
		this.awardPersonUnits = awardPersonUnits;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public Boolean getIsPi() {
		return isPi;
	}

	public void setIsPi(Boolean isPi) {
		this.isPi = isPi;
	}

	public List<AwardPersonAttachment> getAwardPersonAttachment() {
		return awardPersonAttachment;
	}

	public void setAwardPersonAttachment(List<AwardPersonAttachment> awardPersonAttachment) {
		this.awardPersonAttachment = awardPersonAttachment;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public Boolean getIsMultiPi() {
		return isMultiPi;
	}

	public void setIsMultiPi(Boolean isMultiPi) {
		this.isMultiPi = isMultiPi;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

}

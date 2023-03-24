package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.claims.pojo.ClaimManpower;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_MANPOWER_RESOURCE")
@EntityListeners(AuditingEntityListener.class)
public class AwardManpowerResource implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_MANPOWER_RESOURCE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer manpowerResourceId;

	@Column(name = "AWARD_MANPOWER_ID")
	private Integer awardManpowerId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "POSITION_ID")
	private String positionId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "POSITION_STATUS_CODE")
	private String positionStatusCode;

	@Column(name = "COST_ALLOCATION")
	private BigDecimal costAllocation;

	@Column(name = "PLAN_COMPENSATION_TYPE_CODE")
	private String  planCompensationTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK2"), name = "PLAN_COMPENSATION_TYPE_CODE", referencedColumnName = "COMPENSATION_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerCompensationType manpowerPlanCompensationType;

	@Column(name = "PLAN_JOB_PROFILE_TYPE_CODE")
	private String planJobProfileTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK3"), name = "PLAN_JOB_PROFILE_TYPE_CODE", referencedColumnName = "JOB_PROFILE_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerJobProfileType manpowerPlanJobProfileType;

	@Column(name = "COMPENSATION_GRADE_TYPE_CODE")
	private String compensationGradeTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK4"), name = "COMPENSATION_GRADE_TYPE_CODE", referencedColumnName = "COMPENSATION_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerCompensationType manpowerCompensationGradeType;

	@Column(name = "JOB_PROFILE_TYPE_CODE")
	private String jobProfileTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK5"), name = "JOB_PROFILE_TYPE_CODE", referencedColumnName = "JOB_PROFILE_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerJobProfileType manpowerJobProfileType;

	@Column(name = "PLAN_START_DATE")
	private Timestamp planStartDate;

	@Column(name = "PLAN_END_DATE")
	private Timestamp planEndDate;

	@Column(name = "PLAN_DURATION")
	private String planDuration;

	@Column(name = "CHARGE_START_DATE")
	private Timestamp chargeStartDate;

	@Column(name = "CHARGE_END_DATE")
	private Timestamp chargeEndDate;

	@Column(name = "CHARGE_DURATION")
	private String chargeDuration;

	@Column(name = "COMMITTED_COST", precision = 12, scale = 2)
	private BigDecimal committedCost;

	@Column(name = "RESOURCE_TYPE_CODE")
	private String resourceTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK6"), name = "RESOURCE_TYPE_CODE", referencedColumnName = "RESOURCE_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerResourceType manpowerResourceType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "POSITION_OWNED_BY_AWARD")
	private String positionOwnedByAward;

	@Column(name = "CANDIDATE_TITLE_TYPE_CODE")
	private String candidateTitleTypeCode;

	@Column(name = "POSITION_TRIGGER_DATE")
	private Timestamp positionTriggerDate;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK7"), name = "CANDIDATE_TITLE_TYPE_CODE", referencedColumnName = "CANDIDATE_TITLE_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerCandidateTitleType manpowerCandidateTitleType;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK8"), name = "POSITION_STATUS_CODE", referencedColumnName = "POSITION_STATUS_CODE", insertable = false, updatable = false)		
	private ManpowerPositionStatus manpowerPositionStatus;

	@Column(name = "PLANNED_BASE_SALARY", precision = 12, scale = 2)
	private BigDecimal plannedBaseSalary = BigDecimal.ZERO;

	@Column(name = "PLANNED_SALARY", precision = 12, scale = 2)
	private BigDecimal plannedSalary = BigDecimal.ZERO;

	@Column(name = "RESOURCE_UNIQUE_ID")
	private String resourceUniqueId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "FREEZE_DATE")
	private Timestamp freezeDate;

	@Column(name = "UPGRADE_TYPE_CODE")
	private String upgradeTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MANPOWER_RESOURCE_FK9"), name = "UPGRADE_TYPE_CODE", referencedColumnName = "UPGRADE_TYPE_CODE", insertable = false, updatable = false)
	private ManpowerUpgradeType manpowerUpgradeType;

	@Column(name = "MULTIPLIER_USED", precision = 12, scale = 2)
	private BigDecimal multiplierValueUsed;

	@Column(name = "PREVIOUS_CHARGE_END_DATE")
	private Timestamp previousChargeEndDate;

	@Column(name = "PREVIOUS_CHARGE_START_DATE")
	private Timestamp previousChargeStartDate;

	@Column(name = "BASE_SALARY_USED")
	private String baseSalaryUsed;

	@Column(name = "IS_REMAINING_CA_FROM_WBS")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isRemainingCAFromWBS;

	@Column(name = "IS_RESOURCE_CREATED_OR_UPDATED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isResourceCreatedOrUpdated;

	@Transient
	private Manpower manpower;

	@Transient
	private String department;

	@Transient
	private String personStatus;
	
	@Transient
	private String designation;

	@Transient
	private AwardManpower awardManpower;

	@Transient
	private BigDecimal payrollAmount = BigDecimal.ZERO;

	@Transient
	private Boolean isResourceExitInWorkday = Boolean.FALSE;

	@Transient
	private ClaimManpower claimManpower = new ClaimManpower();

	@Transient
	private String organization;

	@Transient
	private String accountNumber;

	@Transient
	private String candidateType;

	@Transient
	private Integer awardId;
	
	@Transient
	private Timestamp involvementFrom;

	@Transient
	private Timestamp involvementTo;
	
	@Transient
	private Timestamp dateOfBirth;

	@Transient
	private Timestamp dateOfInactive;

	@Transient
	private String budgetReferenceNumber;

	@Transient
	private String positionStatus;

	@Transient
	private String submitUser;

	@Transient
	private Integer workdayManpowerInterfaceId;

	@Transient
	private String piName;

	@Transient
	private String leadUnit;

	@Transient
	private String emailAddress;

	@Transient
	private Integer manpowerCount;

	@Transient
	private String involvementPeriod;
	
	@Transient
	private String jobProfileTitle;

	public AwardManpowerResource(String positionId, BigDecimal costAllocation, String awardNumber, String accountNumber) {
		super();
		this.positionId = positionId;
		this.costAllocation = costAllocation;
		this.awardNumber = awardNumber;
		this.accountNumber = accountNumber;
	}

	public AwardManpowerResource(String awardNumber, String positionId) {
		super();
		this.positionId = positionId;
		this.awardNumber = awardNumber;
	}

	public AwardManpowerResource(String personId, String positionId, String awardNumber) {
		this.personId = personId;
		this.positionId = positionId;
		this.awardNumber = awardNumber;
	}

	public AwardManpowerResource() {

	}

	public AwardManpowerResource(String personId, String positionId, String fullName, String emailAddress,
			BigDecimal costAllocation, Date chargeStartDate, Date chargeEndDate, BigDecimal committedCost,
			String awardNumber, BigDecimal multiplierValueUsed, String baseSalaryUsed, String accountNumber) {
		super();
		this.personId = personId;
		this.positionId = positionId;
		this.fullName = fullName;
		this.emailAddress = emailAddress;
		this.costAllocation = costAllocation;
		this.chargeStartDate = chargeStartDate != null? new Timestamp(chargeStartDate.getTime()) : null;
		this.chargeEndDate =chargeEndDate != null?  new Timestamp(chargeEndDate.getTime()) : null;
		this.committedCost = committedCost;
		this.awardNumber = awardNumber;
		this.multiplierValueUsed = multiplierValueUsed;
		this.baseSalaryUsed = baseSalaryUsed;
		this.accountNumber = accountNumber;
	}

	public AwardManpower getAwardManpower() {
		return awardManpower;
	}

	public void setAwardManpower(AwardManpower awardManpower) {
		this.awardManpower = awardManpower;
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

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPositionStatusCode() {
		return positionStatusCode;
	}

	public void setPositionStatusCode(String positionStatusCode) {
		this.positionStatusCode = positionStatusCode;
	}

	public BigDecimal getCostAllocation() {
		return costAllocation;
	}

	public void setCostAllocation(BigDecimal costAllocation) {
		this.costAllocation = costAllocation;
	}

	public ManpowerJobProfileType getManpowerJobProfileType() {
		return manpowerJobProfileType;
	}

	public void setManpowerJobProfileType(ManpowerJobProfileType manpowerJobProfileType) {
		this.manpowerJobProfileType = manpowerJobProfileType;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getPlanStartDate() {
		return planStartDate;
	}

	public void setPlanStartDate(Timestamp planStartDate) {
		this.planStartDate = planStartDate;
	}

	public Timestamp getPlanEndDate() {
		return planEndDate;
	}

	public void setPlanEndDate(Timestamp planEndDate) {
		this.planEndDate = planEndDate;
	}

	public String getPlanDuration() {
		return planDuration;
	}

	public void setPlanDuration(String planDuration) {
		this.planDuration = planDuration;
	}

	public Timestamp getChargeStartDate() {
		return chargeStartDate;
	}

	public void setChargeStartDate(Timestamp chargeStartDate) {
		this.chargeStartDate = chargeStartDate;
	}

	public Timestamp getChargeEndDate() {
		return chargeEndDate;
	}

	public void setChargeEndDate(Timestamp chargeEndDate) {
		this.chargeEndDate = chargeEndDate;
	}

	public String getChargeDuration() {
		return chargeDuration;
	}

	public void setChargeDuration(String chargeDuration) {
		this.chargeDuration = chargeDuration;
	}

	public BigDecimal getCommittedCost() {
		return committedCost;
	}

	public void setCommittedCost(BigDecimal committedCost) {
		this.committedCost = committedCost;
	}

	public ManpowerResourceType getManpowerResourceType() {
		return manpowerResourceType;
	}

	public void setManpowerResourceType(ManpowerResourceType manpowerResourceType) {
		this.manpowerResourceType = manpowerResourceType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getPositionOwnedByAward() {
		return positionOwnedByAward;
	}

	public void setPositionOwnedByAward(String positionOwnedByAward) {
		this.positionOwnedByAward = positionOwnedByAward;
	}

	public String getCandidateTitleTypeCode() {
		return candidateTitleTypeCode;
	}

	public void setCandidateTitleTypeCode(String candidateTitleTypeCode) {
		this.candidateTitleTypeCode = candidateTitleTypeCode;
	}

	public Timestamp getPositionTriggerDate() {
		return positionTriggerDate;
	}

	public void setPositionTriggerDate(Timestamp positionTriggerDate) {
		this.positionTriggerDate = positionTriggerDate;
	}

	public String getResourceTypeCode() {
		return resourceTypeCode;
	}

	public void setResourceTypeCode(String resourceTypeCode) {
		this.resourceTypeCode = resourceTypeCode;
	}

	public String getPlanCompensationTypeCode() {
		return planCompensationTypeCode;
	}

	public void setPlanCompensationTypeCode(String planCompensationTypeCode) {
		this.planCompensationTypeCode = planCompensationTypeCode;
	}

	public String getPlanJobProfileTypeCode() {
		return planJobProfileTypeCode;
	}

	public void setPlanJobProfileTypeCode(String planJobProfileTypeCode) {
		this.planJobProfileTypeCode = planJobProfileTypeCode;
	}

	public String getCompensationGradeTypeCode() {
		return compensationGradeTypeCode;
	}

	public void setCompensationGradeTypeCode(String compensationGradeTypeCode) {
		this.compensationGradeTypeCode = compensationGradeTypeCode;
	}

	public String getJobProfileTypeCode() {
		return jobProfileTypeCode;
	}

	public void setJobProfileTypeCode(String jobProfileTypeCode) {
		this.jobProfileTypeCode = jobProfileTypeCode;
	}

	public Integer getAwardManpowerId() {
		return awardManpowerId;
	}

	public void setAwardManpowerId(Integer awardManpowerId) {
		this.awardManpowerId = awardManpowerId;
	}

	public Integer getManpowerResourceId() {
		return manpowerResourceId;
	}

	public void setManpowerResourceId(Integer manpowerResourceId) {
		this.manpowerResourceId = manpowerResourceId;
	}

	public Manpower getManpower() {
		return manpower;
	}

	public void setManpower(Manpower manpower) {
		this.manpower = manpower;
	}

	public ManpowerPositionStatus getManpowerPositionStatus() {
		return manpowerPositionStatus;
	}

	public void setManpowerPositionStatus(ManpowerPositionStatus manpowerPositionStatus) {
		this.manpowerPositionStatus = manpowerPositionStatus;
	}

	public ManpowerJobProfileType getManpowerPlanJobProfileType() {
		return manpowerPlanJobProfileType;
	}

	public void setManpowerPlanJobProfileType(ManpowerJobProfileType manpowerPlanJobProfileType) {
		this.manpowerPlanJobProfileType = manpowerPlanJobProfileType;
	}

	public ManpowerCompensationType getManpowerCompensationGradeType() {
		return manpowerCompensationGradeType;
	}

	public void setManpowerCompensationGradeType(ManpowerCompensationType manpowerCompensationGradeType) {
		this.manpowerCompensationGradeType = manpowerCompensationGradeType;
	}

	public ManpowerCompensationType getManpowerPlanCompensationType() {
		return manpowerPlanCompensationType;
	}

	public void setManpowerPlanCompensationType(ManpowerCompensationType manpowerPlanCompensationType) {
		this.manpowerPlanCompensationType = manpowerPlanCompensationType;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public ManpowerCandidateTitleType getManpowerCandidateTitleType() {
		return manpowerCandidateTitleType;
	}

	public void setManpowerCandidateTitleType(ManpowerCandidateTitleType manpowerCandidateTitleType) {
		this.manpowerCandidateTitleType = manpowerCandidateTitleType;
	}

	public String getPersonStatus() {
		return personStatus;
	}

	public void setPersonStatus(String personStatus) {
		this.personStatus = personStatus;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

	public BigDecimal getPayrollAmount() {
		return payrollAmount;
	}

	public void setPayrollAmount(BigDecimal payrollAmount) {
		this.payrollAmount = payrollAmount;
	}

	public BigDecimal getPlannedBaseSalary() {
		return plannedBaseSalary;
	}

	public void setPlannedBaseSalary(BigDecimal plannedBaseSalary) {
		this.plannedBaseSalary = plannedBaseSalary;
	}

	public BigDecimal getPlannedSalary() {
		return plannedSalary;
	}

	public void setPlannedSalary(BigDecimal plannedSalary) {
		this.plannedSalary = plannedSalary;
	}

	public Boolean getIsResourceExitInWorkday() {
		return isResourceExitInWorkday;
	}

	public void setIsResourceExitInWorkday(Boolean isResourceExitInWorkday) {
		this.isResourceExitInWorkday = isResourceExitInWorkday;
	}
	
	public ClaimManpower getClaimManpower() {
		return claimManpower;
	}

	public void setClaimManpower(ClaimManpower claimManpower) {
		this.claimManpower = claimManpower;
	}

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public String getOrganization() {
		return organization;
	}

	public void setOrganization(String organization) {
		this.organization = organization;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public Timestamp getFreezeDate() {
		return freezeDate;
	}

	public void setFreezeDate(Timestamp freezeDate) {
		this.freezeDate = freezeDate;
	}

	public String getCandidateType() {
		return candidateType;
	}

	public void setCandidateType(String candidateType) {
		this.candidateType = candidateType;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Timestamp getInvolvementFrom() {
		return involvementFrom;
	}

	public void setInvolvementFrom(Timestamp involvementFrom) {
		this.involvementFrom = involvementFrom;
	}

	public Timestamp getInvolvementTo() {
		return involvementTo;
	}

	public void setInvolvementTo(Timestamp involvementTo) {
		this.involvementTo = involvementTo;
	}

	public Timestamp getDateOfBirth() {
		return dateOfBirth;
	}

	public void setDateOfBirth(Timestamp dateOfBirth) {
		this.dateOfBirth = dateOfBirth;
	}

	public BigDecimal getMultiplierValueUsed() {
		return multiplierValueUsed;
	}

	public void setMultiplierValueUsed(BigDecimal multiplierValueUsed) {
		this.multiplierValueUsed = multiplierValueUsed;
	}

	public Timestamp getPreviousChargeEndDate() {
		return previousChargeEndDate;
	}

	public void setPreviousChargeEndDate(Timestamp previousChargeEndDate) {
		this.previousChargeEndDate = previousChargeEndDate;
	}

	public Timestamp getPreviousChargeStartDate() {
		return previousChargeStartDate;
	}

	public void setPreviousChargeStartDate(Timestamp previousChargeStartDate) {
		this.previousChargeStartDate = previousChargeStartDate;
	}

	public String getBaseSalaryUsed() {
		return baseSalaryUsed;
	}

	public void setBaseSalaryUsed(String baseSalaryUsed) {
		this.baseSalaryUsed = baseSalaryUsed;
	}

	public Timestamp getDateOfInactive() {
		return dateOfInactive;
	}

	public void setDateOfInactive(Timestamp dateOfInactive) {
		this.dateOfInactive = dateOfInactive;
	}

	public Boolean getIsRemainingCAFromWBS() {
		return isRemainingCAFromWBS;
	}

	public void setIsRemainingCAFromWBS(Boolean isRemainingCAFromWBS) {
		this.isRemainingCAFromWBS = isRemainingCAFromWBS;
	}

	public String getUpgradeTypeCode() {
		return upgradeTypeCode;
	}

	public void setUpgradeTypeCode(String upgradeTypeCode) {
		this.upgradeTypeCode = upgradeTypeCode;
	}

	public ManpowerUpgradeType getManpowerUpgradeType() {
		return manpowerUpgradeType;
	}

	public void setManpowerUpgradeType(ManpowerUpgradeType manpowerUpgradeType) {
		this.manpowerUpgradeType = manpowerUpgradeType;
	}

	public String getBudgetReferenceNumber() {
		return budgetReferenceNumber;
	}

	public void setBudgetReferenceNumber(String budgetReferenceNumber) {
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public String getPositionStatus() {
		return positionStatus;
	}

	public void setPositionStatus(String positionStatus) {
		this.positionStatus = positionStatus;
	}

	public String getSubmitUser() {
		return submitUser;
	}

	public void setSubmitUser(String submitUser) {
		this.submitUser = submitUser;
	}

	public Integer getWorkdayManpowerInterfaceId() {
		return workdayManpowerInterfaceId;
	}

	public void setWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId) {
		this.workdayManpowerInterfaceId = workdayManpowerInterfaceId;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public String getLeadUnit() {
		return leadUnit;
	}

	public void setLeadUnit(String leadUnit) {
		this.leadUnit = leadUnit;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public Boolean getIsResourceCreatedOrUpdated() {
		return isResourceCreatedOrUpdated;
	}

	public void setIsResourceCreatedOrUpdated(Boolean isResourceCreatedOrUpdated) {
		this.isResourceCreatedOrUpdated = isResourceCreatedOrUpdated;
	}

	public Integer getManpowerCount() {
		return manpowerCount;
	}

	public void setManpowerCount(Integer manpowerCount) {
		this.manpowerCount = manpowerCount;
	}

	public String getInvolvementPeriod() {
		return involvementPeriod;
	}

	public void setInvolvementPeriod(String involvementPeriod) {
		this.involvementPeriod = involvementPeriod;
	}

	public String getJobProfileTitle() {
		return jobProfileTitle;
	}

	public void setJobProfileTitle(String jobProfileTitle) {
		this.jobProfileTitle = jobProfileTitle;
	}

}

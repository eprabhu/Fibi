package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "WORKDAY_MANPOWER_INTERFACE")
@EntityListeners(AuditingEntityListener.class)
public class WorkdayManpowerInterface implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKDAY_MANPOWER_INTERFACE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_WORKDAY_MANPOWER_INTERFACE_ID_GENERATOR")
	@SequenceGenerator(name="SEQ_WORKDAY_MANPOWER_INTERFACE_ID_GENERATOR", sequenceName = "SEQ_WORKDAY_MANPOWER_INTERFACE_ID_GENERATOR", allocationSize=1)
	private Integer workdayManpowerInterfaceId;

	@Column(name = "AWARD_MANPOWER_RESOURCE_ID")
	private Integer awardManpowerResourceId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKDAY_MANPOWER_INTERFACE_FK1"), name = "AWARD_MANPOWER_RESOURCE_ID", referencedColumnName = "AWARD_MANPOWER_RESOURCE_ID", insertable = false, updatable = false)
	private AwardManpowerResource awardManpowerResource;

	@Column(name = "AWARD_MANPOWER_ID")
	private Integer awardManpowerId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKDAY_MANPOWER_INTERFACE_FK2"), name = "AWARD_MANPOWER_ID", referencedColumnName = "AWARD_MANPOWER_ID", insertable = false, updatable = false)
	private AwardManpower awardManpower;

	@Column(name = "INTERFACE_TYPE_CODE")
	private String interfaceTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKDAY_MANPOWER_INTERFACE_FK3"), name = "INTERFACE_TYPE_CODE", referencedColumnName = "INTERFACE_TYPE_CODE" , insertable = false, updatable = false)
	private ManpowerInterfaceType manpowerInterfaceType;

	@Column(name = "INTERFACE_STATUS_CODE")
	private String interfaceStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKDAY_MANPOWER_INTERFACE_FK4"), name = "INTERFACE_STATUS_CODE", referencedColumnName = "INTERFACE_STATUS_CODE" , insertable = false, updatable = false)
	private ManpowerInterfaceStatus manpowerInterfaceStatus;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "INTERFACE_TIMESTAMP")
	private Timestamp interfaceTimestamp;

	@Column(name = "RESOURCE_UNIQUE_ID")
	private String resourceUniqueId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "NEW_PI_PERSON")
	private String newPIPersonId;

	@Column(name = "OLD_PI_PERSON")
	private String oldPIPersonId;

	@Column(name = "END_DATE_CHANGE")
	private Integer endDateChange;

	@Column(name = "NEW_END_DATE")
	private Timestamp newAwardEndDate;

	@Column(name = "OLD_FREEZE_DATE")
	private Timestamp oldFreezeDate;

	@Column(name = "IS_COST_ALLOCATION_CREATE")
	private String isCostAllocationCreate;

	@Column(name = "OLD_SUPERIOR_SUP_ORG")
	private String oldSuperiorSupOrg;

	@Column(name = "NEW_SUPERIOR_SUP_ORG")
	private String newSuperiorSupOrg;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "MANPOWER_USER_ACTION_CODE")
	private String manpowerUserActionCode;

	@Column(name = "IS_MAIL_ACTIVE")
	private String isMailActive;

	private transient String manpowerUserAction;

	private transient String parentInterfaceStatus;

	public Integer getWorkdayManpowerInterfaceId() {
		return workdayManpowerInterfaceId;
	}

	public void setWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId) {
		this.workdayManpowerInterfaceId = workdayManpowerInterfaceId;
	}

	public AwardManpowerResource getAwardManpowerResource() {
		return awardManpowerResource;
	}

	public void setAwardManpowerResource(AwardManpowerResource awardManpowerResource) {
		this.awardManpowerResource = awardManpowerResource;
	}

	public AwardManpower getAwardManpower() {
		return awardManpower;
	}

	public void setAwardManpower(AwardManpower awardManpower) {
		this.awardManpower = awardManpower;
	}

	public ManpowerInterfaceType getManpowerInterfaceType() {
		return manpowerInterfaceType;
	}

	public void setManpowerInterfaceType(ManpowerInterfaceType manpowerInterfaceType) {
		this.manpowerInterfaceType = manpowerInterfaceType;
	}

	public ManpowerInterfaceStatus getManpowerInterfaceStatus() {
		return manpowerInterfaceStatus;
	}

	public void setManpowerInterfaceStatus(ManpowerInterfaceStatus manpowerInterfaceStatus) {
		this.manpowerInterfaceStatus = manpowerInterfaceStatus;
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

	public Timestamp getInterfaceTimestamp() {
		return interfaceTimestamp;
	}

	public void setInterfaceTimestamp(Timestamp interfaceTimestamp) {
		this.interfaceTimestamp = interfaceTimestamp;
	}

	public String getInterfaceTypeCode() {
		return interfaceTypeCode;
	}

	public void setInterfaceTypeCode(String interfaceTypeCode) {
		this.interfaceTypeCode = interfaceTypeCode;
	}

	public String getInterfaceStatusCode() {
		return interfaceStatusCode;
	}

	public void setInterfaceStatusCode(String interfaceStatusCode) {
		this.interfaceStatusCode = interfaceStatusCode;
	}

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public Integer getAwardManpowerId() {
		return awardManpowerId;
	}

	public void setAwardManpowerId(Integer awardManpowerId) {
		this.awardManpowerId = awardManpowerId;
	}

	public Integer getAwardManpowerResourceId() {
		return awardManpowerResourceId;
	}

	public void setAwardManpowerResourceId(Integer awardManpowerResourceId) {
		this.awardManpowerResourceId = awardManpowerResourceId;
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

	public String getNewPIPersonId() {
		return newPIPersonId;
	}

	public void setNewPIPersonId(String newPIPersonId) {
		this.newPIPersonId = newPIPersonId;
	}

	public String getOldPIPersonId() {
		return oldPIPersonId;
	}

	public void setOldPIPersonId(String oldPIPersonId) {
		this.oldPIPersonId = oldPIPersonId;
	}

	public Integer getEndDateChange() {
		return endDateChange;
	}

	public void setEndDateChange(Integer endDateChange) {
		this.endDateChange = endDateChange;
	}

	public Timestamp getNewAwardEndDate() {
		return newAwardEndDate;
	}

	public void setNewAwardEndDate(Timestamp newAwardEndDate) {
		this.newAwardEndDate = newAwardEndDate;
	}

	public Timestamp getOldFreezeDate() {
		return oldFreezeDate;
	}

	public void setOldFreezeDate(Timestamp oldFreezeDate) {
		this.oldFreezeDate = oldFreezeDate;
	}

	public String getIsCostAllocationCreate() {
		return isCostAllocationCreate;
	}

	public void setIsCostAllocationCreate(String isCostAllocationCreate) {
		this.isCostAllocationCreate = isCostAllocationCreate;
	}

	public String getOldSuperiorSupOrg() {
		return oldSuperiorSupOrg;
	}

	public void setOldSuperiorSupOrg(String oldSuperiorSupOrg) {
		this.oldSuperiorSupOrg = oldSuperiorSupOrg;
	}

	public String getNewSuperiorSupOrg() {
		return newSuperiorSupOrg;
	}

	public void setNewSuperiorSupOrg(String newSuperiorSupOrg) {
		this.newSuperiorSupOrg = newSuperiorSupOrg;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getManpowerUserActionCode() {
		return manpowerUserActionCode;
	}

	public void setManpowerUserActionCode(String manpowerUserActionCode) {
		this.manpowerUserActionCode = manpowerUserActionCode;
	}

	public String getManpowerUserAction() {
		return manpowerUserAction;
	}

	public void setManpowerUserAction(String manpowerUserAction) {
		this.manpowerUserAction = manpowerUserAction;
	}

	public String getParentInterfaceStatus() {
		return parentInterfaceStatus;
	}

	public void setParentInterfaceStatus(String parentInterfaceStatus) {
		this.parentInterfaceStatus = parentInterfaceStatus;
	}

	public String getIsMailActive() {
		return isMailActive;
	}

	public void setIsMailActive(String isMailActive) {
		this.isMailActive = isMailActive;
	}

}

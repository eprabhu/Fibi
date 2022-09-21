package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;

@Entity
@Table(name = "AWARD_MANPOWER")
@EntityListeners(AuditingEntityListener.class)
public class AwardManpower implements Serializable  {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_MANPOWER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardManpowerId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "MANPOWER_TYPE_CODE")
	private String manpowerTypeCode;

	@Column(name = "BUDGET_REFERENCE_NUMBER")
	private String budgetReferenceNumber;

	@Column(name = "BUDGET_REFERENCE_TYPE_CODE")
	private String budgetReferenceTypeCode;

	@Column(name = "BUDGET_VERSION_NUMBER")
	private Integer budgetVersionNumber;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Transient
	private List<AwardManpowerResource> awardManpowerResource;

	@Transient
	private ManpowerType manpowerType;

	@Transient
	private BigDecimal sapCommittedAmount = BigDecimal.ZERO;

	@Transient
	private BudgetCategory budgetCategory;

	@Transient
	private CostElement costElement;

	@Transient
	private BigDecimal budgetAmount = BigDecimal.ZERO;

	@Transient
	private BigDecimal expenseAmount = BigDecimal.ZERO;

	@Transient
	private Integer actualHeadCount;

	@Transient
	private Integer approvedHeadCount;

	public AwardManpower() {
		 awardManpowerResource = new ArrayList<>();
	 }

	public Integer getAwardManpowerId() {
		return awardManpowerId;
	}

	public void setAwardManpowerId(Integer awardManpowerId) {
		this.awardManpowerId = awardManpowerId;
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

	public String getManpowerTypeCode() {
		return manpowerTypeCode;
	}

	public void setManpowerTypeCode(String manpowerTypeCode) {
		this.manpowerTypeCode = manpowerTypeCode;
	}

	public String getBudgetReferenceNumber() {
		return budgetReferenceNumber;
	}

	public void setBudgetReferenceNumber(String budgetReferenceNumber) {
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public String getBudgetReferenceTypeCode() {
		return budgetReferenceTypeCode;
	}

	public void setBudgetReferenceTypeCode(String budgetReferenceTypeCode) {
		this.budgetReferenceTypeCode = budgetReferenceTypeCode;
	}

	public Integer getBudgetVersionNumber() {
		return budgetVersionNumber;
	}

	public void setBudgetVersionNumber(Integer budgetVersionNumber) {
		this.budgetVersionNumber = budgetVersionNumber;
	}

	public Integer getApprovedHeadCount() {
		return approvedHeadCount;
	}

	public void setApprovedHeadCount(Integer approvedHeadCount) {
		this.approvedHeadCount = approvedHeadCount;
	}

	public Integer getActualHeadCount() {
		return actualHeadCount;
	}

	public void setActualHeadCount(Integer actualHeadCount) {
		this.actualHeadCount = actualHeadCount;
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

	public List<AwardManpowerResource> getAwardManpowerResource() {
		return awardManpowerResource;
	}

	public void setAwardManpowerResource(List<AwardManpowerResource> awardManpowerResource) {
		this.awardManpowerResource = awardManpowerResource;
	}

	public ManpowerType getManpowerType() {
		return manpowerType;
	}

	public void setManpowerType(ManpowerType manpowerType) {
		this.manpowerType = manpowerType;
	}

	public BigDecimal getSapCommittedAmount() {
		return sapCommittedAmount;
	}

	public void setSapCommittedAmount(BigDecimal sapCommittedAmount) {
		this.sapCommittedAmount = sapCommittedAmount;
	}

	public BudgetCategory getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(BudgetCategory budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public CostElement getCostElement() {
		return costElement;
	}

	public void setCostElement(CostElement costElement) {
		this.costElement = costElement;
	}

	public BigDecimal getBudgetAmount() {
		return budgetAmount;
	}

	public void setBudgetAmount(BigDecimal budgetAmount) {
		this.budgetAmount = budgetAmount;
	}

	public BigDecimal getExpenseAmount() {
		return expenseAmount;
	}

	public void setExpenseAmount(BigDecimal expenseAmount) {
		this.expenseAmount = expenseAmount;
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

}

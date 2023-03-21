package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "IP_BUDGET_PERIOD")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalBudgetPeriod implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERIOD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_BUD_PERIOD_ID_GNTR")
	@SequenceGenerator(name = "SEQ_IP_BUD_PERIOD_ID_GNTR", sequenceName = "SEQ_IP_BUD_PERIOD_ID_GNTR", allocationSize = 1)
	private Integer budgetPeriodId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "IP_BUDGET_PERIOD_FK1"), name = "BUDGET_HEADER_ID", referencedColumnName = "BUDGET_HEADER_ID")

	private InstituteProposalBudgetHeader budgetHeader;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "BUDGET_PERIOD")
	private Integer budgetPeriod;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "TOTAL_COST", precision = 12, scale = 2)
	private BigDecimal totalCost = BigDecimal.ZERO;

	@Column(name = "TOTAL_DIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalDirectCost = BigDecimal.ZERO;

	@Column(name = "TOTAL_INDIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalIndirectCost = BigDecimal.ZERO;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERIOD_LABEL")
	private String periodLabel;

	@Column(name = "IS_OBLIGATED_PERIOD")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isObligatedPeriod;

	@Column(name = "SUBCONTRACT_COST", precision = 12, scale = 2)
	private BigDecimal subcontractCost = BigDecimal.ZERO;

	@Column(name = "COST_SHARING_AMOUNT", precision = 12, scale = 2)
	private BigDecimal costSharingAmount = BigDecimal.ZERO;

	@Column(name = "UNDERRECOVERY_AMOUNT", precision = 12, scale = 2)
	private BigDecimal underRecoveryAmount = BigDecimal.ZERO;

	@Column(name = "TOTAL_DIRECT_COST_LIMIT", precision = 12, scale = 2)
	private BigDecimal totalDirectCostLimit = BigDecimal.ZERO;

	@Column(name = "TOTAL_COST_LIMIT", precision = 12, scale = 2)
	private BigDecimal totalCostLimit = BigDecimal.ZERO;

	@Column(name = "COMMENTS")
	private String comments;

	@Transient
	private BigDecimal totalOfTotalCost = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;

	@Transient
	private BigDecimal totalInKind = BigDecimal.ZERO;

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public InstituteProposalBudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(InstituteProposalBudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}

	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
	}

	public BigDecimal getTotalDirectCost() {
		return totalDirectCost;
	}

	public void setTotalDirectCost(BigDecimal totalDirectCost) {
		this.totalDirectCost = totalDirectCost;
	}

	public BigDecimal getTotalIndirectCost() {
		return totalIndirectCost;
	}

	public void setTotalIndirectCost(BigDecimal totalIndirectCost) {
		this.totalIndirectCost = totalIndirectCost;
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

	public String getPeriodLabel() {
		return periodLabel;
	}

	public void setPeriodLabel(String periodLabel) {
		this.periodLabel = periodLabel;
	}

	public Boolean getIsObligatedPeriod() {
		return isObligatedPeriod;
	}

	public void setIsObligatedPeriod(Boolean isObligatedPeriod) {
		this.isObligatedPeriod = isObligatedPeriod;
	}

	public BigDecimal getSubcontractCost() {
		return subcontractCost;
	}

	public void setSubcontractCost(BigDecimal subcontractCost) {
		this.subcontractCost = subcontractCost;
	}

	public BigDecimal getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getTotalDirectCostLimit() {
		return totalDirectCostLimit;
	}

	public void setTotalDirectCostLimit(BigDecimal totalDirectCostLimit) {
		this.totalDirectCostLimit = totalDirectCostLimit;
	}

	public BigDecimal getTotalCostLimit() {
		return totalCostLimit;
	}

	public void setTotalCostLimit(BigDecimal totalCostLimit) {
		this.totalCostLimit = totalCostLimit;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public BigDecimal getUnderRecoveryAmount() {
		return underRecoveryAmount;
	}

	public void setUnderRecoveryAmount(BigDecimal underRecoveryAmount) {
		this.underRecoveryAmount = underRecoveryAmount;
	}

	public BigDecimal getTotalOfTotalCost() {
		return totalOfTotalCost;
	}

	public void setTotalOfTotalCost(BigDecimal totalOfTotalCost) {
		this.totalOfTotalCost = totalOfTotalCost;
	}

	public BigDecimal getTotalModifiedDirectCost() {
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(BigDecimal totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
	}

	public BigDecimal getTotalInKind() {
		return totalInKind;
	}

	public void setTotalInKind(BigDecimal totalInKind) {
		this.totalInKind = totalInKind;
	}

}

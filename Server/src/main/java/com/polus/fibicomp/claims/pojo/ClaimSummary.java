package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.budget.pojo.BudgetCategory;

@Entity
@Table(name = "CLAIM_SUMMARY")
@EntityListeners(AuditingEntityListener.class)
public class ClaimSummary implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CLAIM_SUMMARY_ID")
	private Integer claimSummaryId;
	
	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;
	
	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetHeaderId;
	
	@Column(name = "BUDGET_CATEGORY_CODE")
	private String budgetCategoryCode;
	
	@Column(name = "TOTAL_AMOUNT")
	private BigDecimal totalAmount = BigDecimal.ZERO;
	
	@Column(name = "AMOUNT_REQUESTED")
	private BigDecimal amountRequested = BigDecimal.ZERO; 
	
	@Column(name = "AMOUNT_FORCASTED")
	private BigDecimal amountForcasted = BigDecimal.ZERO; 
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;
	
	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimeStamp;
		
	@Column(name = "ORIGINAL_APPROVED_BUDGET")
	private BigDecimal originalApprovedBudget = BigDecimal.ZERO; 

	@Column(name = "LATEST_APPROVED_BUDGET")
	private BigDecimal latestApprovedBudget = BigDecimal.ZERO; 
	
	@Column(name = "PREV_CLAIM_TOTAL_AMOUNT")
	private BigDecimal prevClaimsTotalAmount = BigDecimal.ZERO; 
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_SUMMARY_FK1"), name = "CLAIM_ID", referencedColumnName = "CLAIM_ID", insertable = false, updatable = false)
	private Claim claim;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_SUMMARY_FK3"), name = "BUDGET_CATEGORY_CODE", referencedColumnName = "BUDGET_CATEGORY_CODE", insertable = false, updatable = false)
	private BudgetCategory budgetCategory;
	
	@Column(name = "AMOUNT_REQ_CURRENT_CLAIM", precision = 12, scale = 2)
	private BigDecimal amountReqForCurrentClaim = BigDecimal.ZERO;

	@Column(name = "CUM_CLAIM_AMOUNT_UPTO_CLAIM", precision = 12, scale = 2)
	private BigDecimal cumClaimAmountUptoClaim = BigDecimal.ZERO;

	@Column(name = "CUM_EXPENSE_UPTO_PREV_CLAIM", precision = 12, scale = 2)
	private BigDecimal cumExpenseUptoPrevClaim = BigDecimal.ZERO;

	@Column(name = "COMMITMENTS_UPTO_PREV_CLAIM", precision = 12, scale = 2)
	private BigDecimal commitmentsUptoPrevClaim = BigDecimal.ZERO;

	private transient BigDecimal total = BigDecimal.ZERO;
																																								   
	private transient BigDecimal cumClaimUptoPeriod = BigDecimal.ZERO;

	private transient BigDecimal cumExpenseUptoPrevPeriod = BigDecimal.ZERO;

	private transient BigDecimal expenseIncuredToThisPeriod = BigDecimal.ZERO;

	private transient BigDecimal totalExpenseIncuredUptoPeriod = BigDecimal.ZERO;

	private transient BigDecimal balanceAmtTillPrevPeriod = BigDecimal.ZERO;
	
	private transient BigDecimal comAmtUptoPeriod = BigDecimal.ZERO;
	
	private transient BigDecimal amtReqForCurrentPeroid = BigDecimal.ZERO;

	private transient BigDecimal virementPercentage;

	private transient BigDecimal utilizationPercentage;
	
	private transient BigDecimal originalApprovedBudgetForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal latestApprovedBudgetForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal prevClaimsTotalAmountForOOEMAC = BigDecimal.ZERO;
	
	private transient BigDecimal totalForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal amountRequestedForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal cumClaimUptoPeriodForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal totalExpenseIncuredUptoPeriodForOOEMAC = BigDecimal.ZERO;

	private transient BigDecimal amountForcastedForOOEMAC = BigDecimal.ZERO;

	public BigDecimal getOriginalApprovedBudgetForOOEMAC() {
		return originalApprovedBudgetForOOEMAC;
	}

	public void setOriginalApprovedBudgetForOOEMAC(BigDecimal originalApprovedBudgetForOOEMAC) {
		this.originalApprovedBudgetForOOEMAC = originalApprovedBudgetForOOEMAC;
	}

	public BigDecimal getLatestApprovedBudgetForOOEMAC() {
		return latestApprovedBudgetForOOEMAC;
	}

	public void setLatestApprovedBudgetForOOEMAC(BigDecimal latestApprovedBudgetForOOEMAC) {
		this.latestApprovedBudgetForOOEMAC = latestApprovedBudgetForOOEMAC;
	}

	public BigDecimal getPrevClaimsTotalAmountForOOEMAC() {
		return prevClaimsTotalAmountForOOEMAC;
	}

	public void setPrevClaimsTotalAmountForOOEMAC(BigDecimal prevClaimsTotalAmountForOOEMAC) {
		this.prevClaimsTotalAmountForOOEMAC = prevClaimsTotalAmountForOOEMAC;
	}

	public BigDecimal getTotalForOOEMAC() {
		return totalForOOEMAC;
	}

	public void setTotalForOOEMAC(BigDecimal totalForOOEMAC) {
		this.totalForOOEMAC = totalForOOEMAC;
	}

	public BigDecimal getAmountRequestedForOOEMAC() {
		return amountRequestedForOOEMAC;
	}

	public void setAmountRequestedForOOEMAC(BigDecimal amountRequestedForOOEMAC) {
		this.amountRequestedForOOEMAC = amountRequestedForOOEMAC;
	}

	public Integer getClaimSummaryId() {
		return claimSummaryId;
	}

	public void setClaimSummaryId(Integer claimSummaryId) {
		this.claimSummaryId = claimSummaryId;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public Integer getBudgetHeaderId() {
		return budgetHeaderId;
	}

	public void setBudgetHeaderId(Integer budgetHeaderId) {
		this.budgetHeaderId = budgetHeaderId;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public BigDecimal getTotalAmount() {
		return totalAmount;
	}

	public void setTotalAmount(BigDecimal totalAmount) {
		this.totalAmount = totalAmount;
	}

	public BigDecimal getAmountRequested() {
		return amountRequested;
	}

	public void setAmountRequested(BigDecimal amountRequested) {
		this.amountRequested = amountRequested;
	}

	public BigDecimal getAmountForcasted() {
		return amountForcasted;
	}

	public void setAmountForcasted(BigDecimal amountForcasted) {
		this.amountForcasted = amountForcasted;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public BigDecimal getOriginalApprovedBudget() {
		return originalApprovedBudget;
	}

	public void setOriginalApprovedBudget(BigDecimal originalApprovedBudget) {
		this.originalApprovedBudget = originalApprovedBudget;
	}

	public BigDecimal getLatestApprovedBudget() {
		return latestApprovedBudget;
	}

	public void setLatestApprovedBudget(BigDecimal latestApprovedBudget) {
		this.latestApprovedBudget = latestApprovedBudget;
	}

	public BigDecimal getPrevClaimsTotalAmount() {
		return prevClaimsTotalAmount;
	}

	public void setPrevClaimsTotalAmount(BigDecimal prevClaimsTotalAmount) {
		this.prevClaimsTotalAmount = prevClaimsTotalAmount;
	}

	public Claim getClaim() {
		return claim;
	}

	public void setClaim(Claim claim) {
		this.claim = claim;
	}

	public BudgetCategory getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(BudgetCategory budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public BigDecimal getTotal() {
		return total;
	}

	public void setTotal(BigDecimal total) {
		this.total = total;
	}

	public BigDecimal getCumClaimUptoPeriod() {
		return cumClaimUptoPeriod;
	}

	public void setCumClaimUptoPeriod(BigDecimal cumClaimUptoPeriod) {
		this.cumClaimUptoPeriod = cumClaimUptoPeriod;
	}

	public BigDecimal getCumExpenseUptoPrevPeriod() {
		return cumExpenseUptoPrevPeriod;
	}

	public void setCumExpenseUptoPrevPeriod(BigDecimal cumExpenseUptoPrevPeriod) {
		this.cumExpenseUptoPrevPeriod = cumExpenseUptoPrevPeriod;
	}

	public BigDecimal getExpenseIncuredToThisPeriod() {
		return expenseIncuredToThisPeriod;
	}

	public void setExpenseIncuredToThisPeriod(BigDecimal expenseIncuredToThisPeriod) {
		this.expenseIncuredToThisPeriod = expenseIncuredToThisPeriod;
	}

	public BigDecimal getTotalExpenseIncuredUptoPeriod() {
		return totalExpenseIncuredUptoPeriod;
	}

	public void setTotalExpenseIncuredUptoPeriod(BigDecimal totalExpenseIncuredUptoPeriod) {
		this.totalExpenseIncuredUptoPeriod = totalExpenseIncuredUptoPeriod;
	}

	public BigDecimal getBalanceAmtTillPrevPeriod() {
		return balanceAmtTillPrevPeriod;
	}

	public void setBalanceAmtTillPrevPeriod(BigDecimal balanceAmtTillPrevPeriod) {
		this.balanceAmtTillPrevPeriod = balanceAmtTillPrevPeriod;
	}

	public BigDecimal getComAmtUptoPeriod() {
		return comAmtUptoPeriod;
	}

	public void setComAmtUptoPeriod(BigDecimal comAmtUptoPeriod) {
		this.comAmtUptoPeriod = comAmtUptoPeriod;
	}

	public BigDecimal getAmtReqForCurrentPeroid() {
		return amtReqForCurrentPeroid;
	}

	public void setAmtReqForCurrentPeroid(BigDecimal amtReqForCurrentPeroid) {
		this.amtReqForCurrentPeroid = amtReqForCurrentPeroid;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public BigDecimal getVirementPercentage() {
		return virementPercentage;
	}

	public void setVirementPercentage(BigDecimal virementPercentage) {
		this.virementPercentage = virementPercentage;
	}

	public BigDecimal getUtilizationPercentage() {
		return utilizationPercentage;
	}

	public void setUtilizationPercentage(BigDecimal utilizationPercentage) {
		this.utilizationPercentage = utilizationPercentage;
	}

	public BigDecimal getAmountReqForCurrentClaim() {
		return amountReqForCurrentClaim;
	}

	public void setAmountReqForCurrentClaim(BigDecimal amountReqForCurrentClaim) {
		this.amountReqForCurrentClaim = amountReqForCurrentClaim;
	}
	
	public BigDecimal getCumExpenseUptoPrevClaim() {
		return cumExpenseUptoPrevClaim;
	}

	public void setCumExpenseUptoPrevClaim(BigDecimal cumExpenseUptoPrevClaim) {
		this.cumExpenseUptoPrevClaim = cumExpenseUptoPrevClaim;
	}

	public BigDecimal getCommitmentsUptoPrevClaim() {
		return commitmentsUptoPrevClaim;
	}

	public void setCommitmentsUptoPrevClaim(BigDecimal commitmentsUptoPrevClaim) {
		this.commitmentsUptoPrevClaim = commitmentsUptoPrevClaim;
	}

	public BigDecimal getCumClaimAmountUptoClaim() {
		return cumClaimAmountUptoClaim;
	}

	public void setCumClaimAmountUptoClaim(BigDecimal cumClaimAmountUptoClaim) {
		this.cumClaimAmountUptoClaim = cumClaimAmountUptoClaim;
	}

	public BigDecimal getCumClaimUptoPeriodForOOEMAC() {
		return cumClaimUptoPeriodForOOEMAC;
	}

	public void setCumClaimUptoPeriodForOOEMAC(BigDecimal cumClaimUptoPeriodForOOEMAC) {
		this.cumClaimUptoPeriodForOOEMAC = cumClaimUptoPeriodForOOEMAC;
	}

	public BigDecimal getTotalExpenseIncuredUptoPeriodForOOEMAC() {
		return totalExpenseIncuredUptoPeriodForOOEMAC;
	}

	public void setTotalExpenseIncuredUptoPeriodForOOEMAC(BigDecimal totalExpenseIncuredUptoPeriodForOOEMAC) {
		this.totalExpenseIncuredUptoPeriodForOOEMAC = totalExpenseIncuredUptoPeriodForOOEMAC;
	}

	public BigDecimal getAmountForcastedForOOEMAC() {
		return amountForcastedForOOEMAC;
	}

	public void setAmountForcastedForOOEMAC(BigDecimal amountForcastedForOOEMAC) {
		this.amountForcastedForOOEMAC = amountForcastedForOOEMAC;
	}
}

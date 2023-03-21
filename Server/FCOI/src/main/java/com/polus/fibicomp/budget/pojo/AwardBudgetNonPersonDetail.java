package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AWARD_BUDGET_NON_PERSON_DETAIL")
public class AwardBudgetNonPersonDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_NON_PERSON_DTL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetNonPersonDtlId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUD_NON_PERSON_DTL_FK1"), name = "BUDGET_DETAILS_ID", referencedColumnName = "BUDGET_DETAILS_ID")
	private AwardBudgetDetail awardnonPersonBudgetDetail;

	@Column(name = "LINE_ITEM_NUMBER") 
	private Integer lineItemNumber;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "LINE_ITEM_COST", precision = 12, scale = 2)
	private BigDecimal lineItemCost;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getBudgetNonPersonDtlId() {
		return budgetNonPersonDtlId;
	}

	public void setBudgetNonPersonDtlId(Integer budgetNonPersonDtlId) {
		this.budgetNonPersonDtlId = budgetNonPersonDtlId;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public BigDecimal getLineItemCost() {
		return lineItemCost;
	}

	public void setLineItemCost(BigDecimal lineItemCost) {
		this.lineItemCost = lineItemCost;
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

	public AwardBudgetDetail getAwardnonPersonBudgetDetail() {
		return awardnonPersonBudgetDetail;
	}

	public void setAwardnonPersonBudgetDetail(AwardBudgetDetail awardnonPersonBudgetDetail) {
		this.awardnonPersonBudgetDetail = awardnonPersonBudgetDetail;
	}

}

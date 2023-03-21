package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_TEMPLATE")
public class BudgetTemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_TEMPLATE_ID")
	private Integer budgetTemplateId;

	@Column(name = "COST_ELEMENT")
	private String costElement;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_TEMPLATE_FK1"), name = "COST_ELEMENT", referencedColumnName = "COST_ELEMENT", insertable = false, updatable = false)
	private CostElement costElements;

	@Column(name = "RULE_ID")
	private String ruleId;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "IS_SYSTEM_GENRTED_COST_ELEMENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSystemGeneratedCostElement = false;

	@Column(name = "SYSTEM_GEN_COST_ELEMENT_TYPE")
	private String systemGeneratedCEType;

	@Column(name = "BUDGET_TEMPLATE_TYPE_ID")
	private Integer budgetTemplateTypeId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_TEMPLATE_FK2"), name = "BUDGET_TEMPLATE_TYPE_ID", referencedColumnName = "BUDGET_TEMPLATE_TYPE_ID", insertable = false, updatable = false)
	private BudgetTemplateType budgetTemplateType;

	public Integer getBudgetTemplateId() {
		return budgetTemplateId;
	}

	public void setBudgetTemplateId(Integer budgetTemplateId) {
		this.budgetTemplateId = budgetTemplateId;
	}

	public String getCostElement() {
		return costElement;
	}

	public void setCostElement(String costElement) {
		this.costElement = costElement;
	}

	public CostElement getCostElements() {
		return costElements;
	}

	public void setCostElements(CostElement costElements) {
		this.costElements = costElements;
	}

	public String getRuleId() {
		return ruleId;
	}

	public void setRuleId(String ruleId) {
		this.ruleId = ruleId;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
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

	public Boolean getIsSystemGeneratedCostElement() {
		return isSystemGeneratedCostElement;
	}

	public void setIsSystemGeneratedCostElement(Boolean isSystemGeneratedCostElement) {
		this.isSystemGeneratedCostElement = isSystemGeneratedCostElement;
	}

	public String getSystemGeneratedCEType() {
		return systemGeneratedCEType;
	}

	public void setSystemGeneratedCEType(String systemGeneratedCEType) {
		this.systemGeneratedCEType = systemGeneratedCEType;
	}

	public Integer getBudgetTemplateTypeId() {
		return budgetTemplateTypeId;
	}

	public void setBudgetTemplateTypeId(Integer budgetTemplateTypeId) {
		this.budgetTemplateTypeId = budgetTemplateTypeId;
	}

	public BudgetTemplateType getBudgetTemplateType() {
		return budgetTemplateType;
	}

	public void setBudgetTemplateType(BudgetTemplateType budgetTemplateType) {
		this.budgetTemplateType = budgetTemplateType;
	}

}

package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.budget.common.pojo.ValidCeRateType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COST_ELEMENT")
public class CostElement implements Serializable, Comparable<CostElement> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COST_ELEMENT")
	private String costElement;

	@Column(name = "BUDGET_CATEGORY_CODE")
	private String budgetCategoryCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "COST_ELEMENT_FK1"), name = "BUDGET_CATEGORY_CODE", referencedColumnName = "BUDGET_CATEGORY_CODE", insertable = false, updatable = false)
	private BudgetCategory budgetCategory;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "costElementBo", cascade = { CascadeType.ALL }, fetch = FetchType.EAGER)
    private List<ValidCeRateType> validCeRateTypes;

	@JsonManagedReference
	@OneToMany(mappedBy = "ce",  cascade = { CascadeType.REMOVE, CascadeType.ALL }, fetch = FetchType.EAGER)
    private Set<CostElementRate> ceRate;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean onOffCampusFlag;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean active;

	@Column(name = "FIN_OBJECT_CODE")
	private String financialObjectCode;

	@Column(name = "TBN_ID")
	private String tbnId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "COST_ELEMENT_FK2"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String costElementDetail;

	@Transient
	private String systemGeneratedCEType;

	@Override
	public int compareTo(CostElement costElement) {
		return this.costElement.compareTo(costElement.costElement);
	}

	public String getCostElement() {
		return costElement;
	}

	public void setCostElement(String costElement) {
		this.costElement = costElement;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public BudgetCategory getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(BudgetCategory budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(Boolean onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
	}

	public String getFinancialObjectCode() {
		return financialObjectCode;
	}

	public void setFinancialObjectCode(String financialObjectCode) {
		this.financialObjectCode = financialObjectCode;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<ValidCeRateType> getValidCeRateTypes() {
		return validCeRateTypes;
	}

	public void setValidCeRateTypes(List<ValidCeRateType> validCeRateTypes) {
		this.validCeRateTypes = validCeRateTypes;
	}

	public String getCostElementDetail() {
		if (costElement != null && !costElement.isEmpty() && description != null && !description.isEmpty()) {
			costElementDetail = costElement + " - " + description;
		}
		return costElementDetail;
	}

	public void setCostElementDetail(String costElementDetail) {
		this.costElementDetail = costElementDetail;
	}

	public String getSystemGeneratedCEType() {
		return systemGeneratedCEType;
	}

	public void setSystemGeneratedCEType(String systemGeneratedCEType) {
		this.systemGeneratedCEType = systemGeneratedCEType;
	}

	public Set<CostElementRate> getCeRate() {
		return ceRate;
	}

	public void setCeRate(Set<CostElementRate> ceRate) {
		this.ceRate = ceRate;
	}

	public String getTbnId() {
		return tbnId;
	}

	public void setTbnId(String tbnId) {
		this.tbnId = tbnId;
	}

	public TbnPerson getTbnPerson() {
		return tbnPerson;
	}

	public void setTbnPerson(TbnPerson tbnPerson) {
		this.tbnPerson = tbnPerson;
	}
	
}

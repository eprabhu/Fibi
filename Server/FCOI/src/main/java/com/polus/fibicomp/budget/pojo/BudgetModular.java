package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "BUDGET_MODULAR")
public class BudgetModular implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERIOD_ID")
	private Integer budgetPeriodId;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetId;

	@Column(name = "DIRECT_COST_LESS_CONSOR_FNA", precision = 12, scale = 2)
	private BigDecimal directCostLessConsorFna;

	@Column(name = "CONSORTIUM_FNA", precision = 12, scale = 2)
	private BigDecimal consortiumFna;

	@Column(name = "TOTAL_DIRECT_COST", precision = 12, scale = 2)
	private BigDecimal totalDirectCost;

	@Transient
	private Timestamp endDate;

	@Transient
	private Timestamp startDate;

	@Transient
	private BigDecimal totalIndirectCost;

	@Transient
	private BigDecimal totalDirectAndInDirectCost;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "budgetModular", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<BudgetModularIDC> idc;

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public List<BudgetModularIDC> getIdc() {
		return idc;
	}

	public void setIdc(List<BudgetModularIDC> idc) {
		this.idc = idc;
	}

	public BigDecimal getTotalIndirectCost() {
		return totalIndirectCost;
	}

	public void setTotalIndirectCost(BigDecimal totalIndirectCost) {
		this.totalIndirectCost = totalIndirectCost;
	}

	public BigDecimal getTotalDirectAndInDirectCost() {
		return totalDirectAndInDirectCost;
	}

	public void setTotalDirectAndInDirectCost(BigDecimal totalDirectAndInDirectCost) {
		this.totalDirectAndInDirectCost = totalDirectAndInDirectCost;
	}

	public BigDecimal getDirectCostLessConsorFna() {
		return directCostLessConsorFna;
	}

	public void setDirectCostLessConsorFna(BigDecimal directCostLessConsorFna) {
		this.directCostLessConsorFna = directCostLessConsorFna;
	}

	public BigDecimal getConsortiumFna() {
		return consortiumFna;
	}

	public void setConsortiumFna(BigDecimal consortiumFna) {
		this.consortiumFna = consortiumFna;
	}

	public BigDecimal getTotalDirectCost() {
		return totalDirectCost;
	}

	public void setTotalDirectCost(BigDecimal totalDirectCost) {
		this.totalDirectCost = totalDirectCost;
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

}

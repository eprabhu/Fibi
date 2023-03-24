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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "BUDGET_MODULAR_IDC")
public class BudgetModularIDC implements Serializable {
	/*
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_MODULAR_IDC_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_MODULAR_IDC_ID_GENERATOR")
	@SequenceGenerator(name="BUDGET_MODULAR_IDC_ID_GENERATOR", sequenceName = "BUDGET_MODULAR_IDC_ID_GENERATOR", allocationSize=1)
	private Integer budgetModularIDCId;
	
	//@Column(name = "BUDGET_PERIOD_ID")
	//private Integer budgetPeriodId;
	
	@JsonBackReference
    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_MODULAR_IDC_FK1"), name = "BUDGET_PERIOD_ID", referencedColumnName = "BUDGET_PERIOD_ID")
    private BudgetModular budgetModular;
		
	@Column(name = "DESCRIPTION")
	private Integer description;
	
	@Column(name = "IDC_RATE", precision = 5, scale = 2)
	private BigDecimal idcRate;
	
	@Column(name = "IDC_BASE", precision = 12, scale = 2)
	private BigDecimal idcBase;
	
	@Column(name = "FUNDS_REQUESTED", precision = 12, scale = 2)
	private BigDecimal fundsRequested;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getBudgetModularIDCId() {
		return budgetModularIDCId;
	}

	public void setBudgetModularIDCId(Integer budgetModularIDCId) {
		this.budgetModularIDCId = budgetModularIDCId;
	}

//	public Integer getBudgetPeriodId() {
//		return budgetPeriodId;
//	}
//
//	public void setBudgetPeriodId(Integer budgetPeriodId) {
//		this.budgetPeriodId = budgetPeriodId;
//	}

	public BudgetModular getBudgetModular() {
		return budgetModular;
	}

	public void setBudgetModular(BudgetModular budgetModular) {
		this.budgetModular = budgetModular;
	}

	public Integer getDescription() {
		return description;
	}

	public void setDescription(Integer description) {
		this.description = description;
	}

	public BigDecimal getIdcRate() {
		return idcRate;
	}

	public void setIdcRate(BigDecimal idcRate) {
		this.idcRate = idcRate;
	}

	public BigDecimal getIdcBase() {
		return idcBase;
	}

	public void setIdcBase(BigDecimal idcBase) {
		this.idcBase = idcBase;
	}

	public BigDecimal getFundsRequested() {
		return fundsRequested;
	}

	public void setFundsRequested(BigDecimal fundsRequested) {
		this.fundsRequested = fundsRequested;
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
	
	
	
} 
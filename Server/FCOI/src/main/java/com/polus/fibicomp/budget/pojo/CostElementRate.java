package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "COST_ELEMENT_RATE")
public class CostElementRate implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COST_ELEMENT_RATE_ID")
	private Integer costElementRateId;

	@Column(name = "COST_ELEMENT")
	private String costElement;

	@Column(name = "RATE", precision = 12, scale = 2)
	private BigDecimal rate;

	@Column(name = "RATE_TYPE")
	private String rateType;

	@JsonBackReference
	@ManyToOne(optional = false, cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "COST_ELEMENT_RATE_FK1"), name = "COST_ELEMENT", referencedColumnName = "COST_ELEMENT", insertable = false, updatable = false)
	private CostElement ce;

	public Integer getCostElementRateId() {
		return costElementRateId;
	}

	public void setCostElementRateId(Integer costElementRateId) {
		this.costElementRateId = costElementRateId;
	}

	public String getCostElement() {
		return costElement;
	}

	public void setCostElement(String costElement) {
		this.costElement = costElement;
	}

	public CostElement getCe() {
		return ce;
	}

	public void setCe(CostElement ce) {
		this.ce = ce;
	}

	public String getRateType() {
		return rateType;
	}

	public void setRateType(String rateType) {
		this.rateType = rateType;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public BigDecimal getRate() {
		return rate;
	}

	public void setRate(BigDecimal rate) {
		this.rate = rate;
	}

}

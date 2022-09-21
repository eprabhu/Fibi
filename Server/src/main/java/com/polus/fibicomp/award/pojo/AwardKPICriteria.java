package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
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
import com.polus.fibicomp.grantcall.pojo.KPICriteriaType;

@Entity
@Table(name = "AWARD_KPI_CRITERIA")
public class AwardKPICriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_KPI_CRITERIA_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardKPICriteriaId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_KPI_CRITERIA_FK1"), name = "AWARD_KPI_ID", referencedColumnName = "AWARD_KPI_ID")
	private AwardKPI awardKPI;

	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_KPI_CRITERIA_FK2"), name = "KPI_CRITERIA_TYPE_CODE", referencedColumnName = "KPI_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private KPICriteriaType kpiCriteriaType;

	@Column(name = "TARGET")
	private Integer target;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getAwardKPICriteriaId() {
		return awardKPICriteriaId;
	}

	public void setAwardKPICriteriaId(Integer awardKPICriteriaId) {
		this.awardKPICriteriaId = awardKPICriteriaId;
	}

	public AwardKPI getAwardKPI() {
		return awardKPI;
	}

	public void setAwardKPI(AwardKPI awardKPI) {
		this.awardKPI = awardKPI;
	}

	public String getKpiCriteriaTypeCode() {
		return kpiCriteriaTypeCode;
	}

	public void setKpiCriteriaTypeCode(String kpiCriteriaTypeCode) {
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
	}

	public KPICriteriaType getKpiCriteriaType() {
		return kpiCriteriaType;
	}

	public void setKpiCriteriaType(KPICriteriaType kpiCriteriaType) {
		this.kpiCriteriaType = kpiCriteriaType;
	}

	public Integer getTarget() {
		return target;
	}

	public void setTarget(Integer target) {
		this.target = target;
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

}

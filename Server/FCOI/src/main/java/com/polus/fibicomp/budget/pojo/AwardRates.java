package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.adminportal.pojo.RateClass;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_RATES")
public class AwardRates implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_RATE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardRateId;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_RATE_FK3"), name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false)
	private RateClass rateClass;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumns(foreignKey = @ForeignKey(name = "AWARD_RATE_FK4"), value = {
			@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType rateType;

	@Column(name = "FISCAL_YEAR")
	private String fiscalYear;

	@Column(name = "ON_OFF_CAMPUS_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean onOffCampusFlag;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "INSTITUTE_RATE", precision = 12, scale = 2)
	private BigDecimal instituteRate;

	@Column(name = "APPLICABLE_RATE", precision = 12, scale = 2)
	private BigDecimal applicableRate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_RATE_FK1"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private ActivityType activityType;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetHeaderId;

	public String getRateClassCode() {
		return rateClassCode;
	}

	public void setRateClassCode(String rateClassCode) {
		this.rateClassCode = rateClassCode;
	}

	public String getRateTypeCode() {
		return rateTypeCode;
	}

	public void setRateTypeCode(String rateTypeCode) {
		this.rateTypeCode = rateTypeCode;
	}

	public String getFiscalYear() {
		return fiscalYear;
	}

	public void setFiscalYear(String fiscalYear) {
		this.fiscalYear = fiscalYear;
	}

	public Boolean getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(Boolean onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
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

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public RateClass getRateClass() {
		return rateClass;
	}

	public void setRateClass(RateClass rateClass) {
		this.rateClass = rateClass;
	}

	public RateType getRateType() {
		return rateType;
	}

	public void setRateType(RateType rateType) {
		this.rateType = rateType;
	}

	public ActivityType getActivityType() {
		return activityType;
	}

	public void setActivityType(ActivityType activityType) {
		this.activityType = activityType;
	}

	public Integer getAwardRateId() {
		return awardRateId;
	}

	public void setAwardRateId(Integer awardRateId) {
		this.awardRateId = awardRateId;
	}

	public Integer getBudgetHeaderId() {
		return budgetHeaderId;
	}

	public void setBudgetHeaderId(Integer budgetHeaderId) {
		this.budgetHeaderId = budgetHeaderId;
	}

	public BigDecimal getApplicableRate() {
		return applicableRate;
	}

	public void setApplicableRate(BigDecimal applicableRate) {
		this.applicableRate = applicableRate;
	}

	public BigDecimal getInstituteRate() {
		return instituteRate;
	}

	public void setInstituteRate(BigDecimal instituteRate) {
		this.instituteRate = instituteRate;
	}

}

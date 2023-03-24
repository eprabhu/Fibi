package com.polus.fibicomp.adminportal.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name="INSTITUTE_LA_RATES")
public class InstituteLARate implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "InstituteLAIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "InstituteLAIdGenerator")
	@Column(name = "INSTITUTE_LA_RATE_ID")
	private Long id;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean active;

	@Column(name="UNIT_NUMBER")
	private String unitNumber;

	@Column(name="RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name="FISCAL_YEAR")
	private String fiscalYear;

	@Column(name="START_DATE")
	private Timestamp  startDate;

	@Column(name="ON_OFF_CAMPUS_FLAG")
	private String onOffCampusFlag;

	@SuppressWarnings("unchecked")
	private String campusFlag;

	@Column(name = "RATE", precision = 10, scale = 2)
	private BigDecimal instituteRate;

	@Column(name="UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name="UPDATE_USER")
	private String updateUser;

	@Column(name="RATE_CLASS_CODE")
	private String rateClassCode;

	@ManyToOne(optional=true)
	@JoinColumn(foreignKey = @ForeignKey(name="INSTITUTE_LA_RATES_FK1"),name="UNIT_NUMBER", referencedColumnName="UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

    @ManyToOne(cascade = { CascadeType.REFRESH })
    @JoinColumn(foreignKey = @ForeignKey(name = "INSTITUTE_LA_RATES_FK2"), name="RATE_CLASS_CODE", referencedColumnName="RATE_CLASS_CODE", insertable = false, updatable = false)
    private RateClass rateClass;

    @ManyToOne(cascade = { CascadeType.REFRESH })
    @JoinColumns(foreignKey = @ForeignKey(name = "INSTITUTE_LA_RATES_FK3"), value = { 
    	@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false), 
    	@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
    private RateType rateType;

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
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

	public String isOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public void setOnOffCampusFlag(String onOffCampusFlag) {
		this.onOffCampusFlag = onOffCampusFlag;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getOnOffCampusFlag() {
		return onOffCampusFlag;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getRateClassCode() {
		return rateClassCode;
	}

	public void setRateClassCode(String rateClassCode) {
		this.rateClassCode = rateClassCode;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
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

	public String getCampusFlag() {
		if(onOffCampusFlag == "N"){
			return "ON";
		}
		return "OFF";
	}
	
	public void setCampusFlag(String campusFlag) {
		this.campusFlag = campusFlag;
	}

	public BigDecimal getInstituteRate() {
		return instituteRate;
	}

	public void setInstituteRate(BigDecimal instituteRate) {
		this.instituteRate = instituteRate;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

}

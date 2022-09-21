package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "KPI_CRITERIA_TYPE")
public class KPICriteriaType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "kpiCriteriaTypeCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "kpiCriteriaTypeCodeGenerator")
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;

	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_CRITERIA_TYPE_FK"), name = "KPI_TYPE_CODE", referencedColumnName = "KPI_TYPE_CODE", insertable = false, updatable = false)
	private KPIType kpiType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public KPICriteriaType() {
		super();
	}

	public KPICriteriaType(String kpiCriteriaTypeCode, String description) {
		super();
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
		this.description = description;
	}

	public String getKpiCriteriaTypeCode() {
		return kpiCriteriaTypeCode;
	}

	public void setKpiCriteriaTypeCode(String kpiCriteriaTypeCode) {
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
	}

	public KPIType getKpiType() {
		return kpiType;
	}

	public void setKpiType(KPIType kpiType) {
		this.kpiType = kpiType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getKpiTypeCode() {
		return kpiTypeCode;
	}

	public void setKpiTypeCode(String kpiTypeCode) {
		this.kpiTypeCode = kpiTypeCode;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}
    
	
}

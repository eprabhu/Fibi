package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "KPI_TYPE")
public class KPIType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "kpiTypeCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "kpiTypeCodeGenerator")
	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@JsonManagedReference
	@OneToMany(mappedBy = "kpiType", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<KPICriteriaType> kpiCriteriaType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public KPIType() {
		super();
	}

	public KPIType(String kpiTypeCode, String description) {
		super();
		this.kpiTypeCode = kpiTypeCode;
		this.description = description;
	}

	public String getKpiTypeCode() {
		return kpiTypeCode;
	}

	public void setKpiTypeCode(String kpiTypeCode) {
		this.kpiTypeCode = kpiTypeCode;
	}

	public List<KPICriteriaType> getKpiCriteriaType() {
		return kpiCriteriaType;
	}

	public void setKpiCriteriaType(List<KPICriteriaType> kpiCriteriaType) {
		this.kpiCriteriaType = kpiCriteriaType;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}
    
	
}

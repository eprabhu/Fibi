package com.polus.fibicomp.grantcall.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "GRANT_CALL_KPI_CRITERIA")
public class GrantCallKPICriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CALL_KPI_CRITERIA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_KPI_CRITERIA_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_CALL_KPI_CRITERIA_ID_GENERATOR", sequenceName = "GRANT_CALL_KPI_CRITERIA_ID_GENERATOR", allocationSize=1)
	private Integer grantCallKpiCriteriaId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KPI_CRITERIA_FK1"), name = "GRANT_CALL_KPI_ID", referencedColumnName = "GRANT_CALL_KPI_ID")
	private GrantCallKPI grantCallKpi;

	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KPI_CRITERIA_FK2"), name = "KPI_CRITERIA_TYPE_CODE", referencedColumnName = "KPI_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private KPICriteriaType kpiCriteriaType;

	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String description;

	public GrantCallKPI getGrantCallKpi() {
		return grantCallKpi;
	}

	public void setGrantCallKpi(GrantCallKPI grantCallKpi) {
		this.grantCallKpi = grantCallKpi;
	}

	public String getKpiCriteriaTypeCode() {
		return kpiCriteriaTypeCode;
	}

	public void setKpiCriteriaTypeCode(String kpiCriteriaTypeCode) {
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
	}

	public String getKpiTypeCode() {
		return kpiTypeCode;
	}

	public void setKpiTypeCode(String kpiTypeCode) {
		this.kpiTypeCode = kpiTypeCode;
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

	public Integer getGrantCallKpiCriteriaId() {
		return grantCallKpiCriteriaId;
	}

	public void setGrantCallKpiCriteriaId(Integer grantCallKpiCriteriaId) {
		this.grantCallKpiCriteriaId = grantCallKpiCriteriaId;
	}

	public String getDescription() {
		if(this.kpiCriteriaType != null) {
			setDescription(this.kpiCriteriaType.getDescription());
		}
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public KPICriteriaType getKpiCriteriaType() {
		return kpiCriteriaType;
	}

	public void setKpiCriteriaType(KPICriteriaType kpiCriteriaType) {
		this.kpiCriteriaType = kpiCriteriaType;
	}

}

package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "GRANT_CALL_KPI")
@EntityListeners(AuditingEntityListener.class)
public class GrantCallKPI implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CALL_KPI_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_KPI_ID_GENERATOR")
	@SequenceGenerator(name = "GRANT_CALL_KPI_ID_GENERATOR", sequenceName = "GRANT_CALL_KPI_ID_GENERATOR", allocationSize = 1)
	private Integer grantCallKpiId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KPI_FK"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private GrantCall grantCall;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KPI_FK2"), name = "KPI_TYPE_CODE", referencedColumnName = "KPI_TYPE_CODE", insertable = false, updatable = false)
	private KPIType kpiType;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "grantCallKpi", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<GrantCallKPICriteria> grantCallKpiCriterias;

	@Transient
	private String description;

	public Integer getGrantCallKpiId() {
		return grantCallKpiId;
	}

	public void setGrantCallKpiId(Integer grantCallKpiId) {
		this.grantCallKpiId = grantCallKpiId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
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

	public List<GrantCallKPICriteria> getGrantCallKpiCriterias() {
		return grantCallKpiCriterias;
	}

	public void setGrantCallKpiCriterias(List<GrantCallKPICriteria> grantCallKpiCriterias) {
		this.grantCallKpiCriterias = grantCallKpiCriterias;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getDescription() {
		if (this.kpiType != null) {
			setDescription(this.kpiType.getDescription());
		}
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public KPIType getKpiType() {
		return kpiType;
	}

	public void setKpiType(KPIType kpiType) {
		this.kpiType = kpiType;
	}

}

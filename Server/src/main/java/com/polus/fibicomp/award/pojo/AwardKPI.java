package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.grantcall.pojo.KPIType;

@Entity
@Table(name = "AWARD_KPI")
public class AwardKPI implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_KPI_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardKPIId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_KPI_FK1"), name = "KPI_TYPE_CODE", referencedColumnName = "KPI_TYPE_CODE", insertable = false, updatable = false)
	private KPIType kpiType;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "awardKPI", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardKPICriteria> awardKPICriterias;

	public Integer getAwardKPIId() {
		return awardKPIId;
	}

	public void setAwardKPIId(Integer awardKPIId) {
		this.awardKPIId = awardKPIId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getKpiTypeCode() {
		return kpiTypeCode;
	}

	public void setKpiTypeCode(String kpiTypeCode) {
		this.kpiTypeCode = kpiTypeCode;
	}

	public KPIType getKpiType() {
		return kpiType;
	}

	public void setKpiType(KPIType kpiType) {
		this.kpiType = kpiType;
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

	public List<AwardKPICriteria> getAwardKPICriterias() {
		return awardKPICriterias;
	}

	public void setAwardKPICriterias(List<AwardKPICriteria> awardKPICriterias) {
		this.awardKPICriterias = awardKPICriterias;
	}

}

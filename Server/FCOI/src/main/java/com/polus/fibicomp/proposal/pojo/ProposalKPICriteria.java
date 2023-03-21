package com.polus.fibicomp.proposal.pojo;

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
import com.polus.fibicomp.grantcall.pojo.KPICriteriaType;

@Entity
@Table(name = "EPS_PROPOSAL_KPI_CRITERIA")
public class ProposalKPICriteria implements Serializable {
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EPS_PROPOSAL_KPI_CRITERIA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_KPI_CRITERIA_ID_GENERATOR")
	@SequenceGenerator(name = "EPS_PROPOSAL_KPI_CRITERIA_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_KPI_CRITERIA_ID_GENERATOR", allocationSize = 1)
	private Integer proposalKpiCriteriaId;

	@Column(name = "PROPOSAL_KPI_ID")
	private Integer proposalKpiId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_KPI_CRITERIA_FK1"), name = "PROPOSAL_KPI_ID", referencedColumnName = "PROPOSAL_KPI_ID", insertable = false, updatable = false)
	private ProposalKPI proposalKpi;

	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_KPI_CRITERIA_FK3"), name = "KPI_CRITERIA_TYPE_CODE", referencedColumnName = "KPI_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private KPICriteriaType kpiCriteriaType;

	@Column(name = "TARGET")
	private Integer target;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String kpiTypeCode;

	public Integer getProposalKpiCriteriaId() {
		return proposalKpiCriteriaId;
	}

	public void setProposalKpiCriteriaId(Integer proposalKpiCriteriaId) {
		this.proposalKpiCriteriaId = proposalKpiCriteriaId;
	}

	public ProposalKPI getProposalKpi() {
		return proposalKpi;
	}

	public void setProposalKpi(ProposalKPI proposalKpi) {
		this.proposalKpi = proposalKpi;
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

	public Integer getProposalKpiId() {
		return proposalKpiId;
	}

	public void setProposalKpiId(Integer proposalKpiId) {
		this.proposalKpiId = proposalKpiId;
	}

	public String getKpiTypeCode() {
		return kpiTypeCode;
	}

	public void setKpiTypeCode(String kpiTypeCode) {
		this.kpiTypeCode = kpiTypeCode;
	}

}

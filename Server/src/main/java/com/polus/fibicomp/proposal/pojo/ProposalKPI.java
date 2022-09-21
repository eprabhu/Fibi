package com.polus.fibicomp.proposal.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.grantcall.pojo.KPIType;

@Entity
@Table(name = "EPS_PROPOSAL_KPI")
public class ProposalKPI implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_KPI_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_KPI_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROPOSAL_KPI_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_KPI_ID_GENERATOR", allocationSize=1)
	private Integer proposalKpiId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_KPI_FK"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false)
	private Proposal proposal;

	@Column(name = "KPI_TYPE_CODE")
	private String kpiTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_KPI_FK2"), name = "KPI_TYPE_CODE", referencedColumnName = "KPI_TYPE_CODE", insertable = false, updatable = false)
	private KPIType kpiType;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalKpi", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ProposalKPICriteria> proposalKPICriterias;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public ProposalKPI() {
		super();
	}

	public ProposalKPI(Integer proposalKpiId) {
		super();
		this.proposalKpiId = proposalKpiId;
	}

	public Integer getProposalKpiId() {
		return proposalKpiId;
	}

	public void setProposalKpiId(Integer proposalKpiId) {
		this.proposalKpiId = proposalKpiId;
	}

	public Proposal getProposal() {
		return proposal;
	}

	public void setProposal(Proposal proposal) {
		this.proposal = proposal;
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

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public List<ProposalKPICriteria> getProposalKPICriterias() {
		return proposalKPICriterias;
	}

	public void setProposalKPICriterias(List<ProposalKPICriteria> proposalKPICriterias) {
		this.proposalKPICriterias = proposalKPICriterias;
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

}

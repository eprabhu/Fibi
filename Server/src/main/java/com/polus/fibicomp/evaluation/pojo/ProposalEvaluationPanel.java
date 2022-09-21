package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
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
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;
import com.polus.fibicomp.workflow.pojo.WorkflowMap;

@Entity
@Table(name = "EPS_PROPOSAL_EVALUATION_PANEL")
public class ProposalEvaluationPanel implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_EVALUATION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PROPOSAL_EVALUATION_ID_GENERATOR")
	@SequenceGenerator(name = "PROPOSAL_EVALUATION_ID_GENERATOR", sequenceName = "PROPOSAL_EVALUATION_ID_GENERATOR", allocationSize = 1)
	private Integer proposalEvaluationId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "MAP_ID")
	private Integer mapId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_EVALUATION_FK2"), name = "MAP_ID", referencedColumnName = "MAP_ID", insertable = false, updatable = false)
	private WorkflowMap workflowMap;

	@Column(name = "CAN_SCORE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean canScore = false;

	@Column(name = "IS_ADMIN_SELECTED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isAdminSelected = false;

	@Column(name = "IS_PI_SELECTED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPiSelected = false;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp; 

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalEvaluationPanel", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ProposalEvaluationPanelPersons> proposalEvaluationPanelPersons;

	@Transient
	private String panelName;

	public ProposalEvaluationPanel() {
		proposalEvaluationPanelPersons = new ArrayList<ProposalEvaluationPanelPersons>();
	}

	public Integer getProposalEvaluationId() {
		return proposalEvaluationId;
	}

	public void setProposalEvaluationId(Integer proposalEvaluationId) {
		this.proposalEvaluationId = proposalEvaluationId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public WorkflowMap getWorkflowMap() {
		return workflowMap;
	}

	public void setWorkflowMap(WorkflowMap workflowMap) {
		this.workflowMap = workflowMap;
	}

	public Boolean getCanScore() {
		return canScore;
	}

	public void setCanScore(Boolean canScore) {
		this.canScore = canScore;
	}

	public Boolean getIsAdminSelected() {
		return isAdminSelected;
	}

	public void setIsAdminSelected(Boolean isAdminSelected) {
		this.isAdminSelected = isAdminSelected;
	}

	public Boolean getIsPiSelected() {
		return isPiSelected;
	}

	public void setIsPiSelected(Boolean isPiSelected) {
		this.isPiSelected = isPiSelected;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getPanelName() {
		return panelName;
	}

	public void setPanelName(String panelName) {
		this.panelName = panelName;
	}

	public List<ProposalEvaluationPanelPersons> getProposalEvaluationPanelPersons() {
		return proposalEvaluationPanelPersons;
	}

	public void setProposalEvaluationPanelPersons(List<ProposalEvaluationPanelPersons> proposalEvaluationPanelPersons) {
		this.proposalEvaluationPanelPersons = proposalEvaluationPanelPersons;
	}

}

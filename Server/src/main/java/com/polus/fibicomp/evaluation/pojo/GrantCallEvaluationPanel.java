package com.polus.fibicomp.evaluation.pojo;

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
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.workflow.pojo.WorkflowMap;

@Entity
@Table(name = "GRANT_CALL_EVALUATION_PANEL")
public class GrantCallEvaluationPanel implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CALL_EVALUATION_PANEL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_EVALUATION_PANEL_ID_GENERATOR")
	@SequenceGenerator(name = "GRANT_CALL_EVALUATION_PANEL_ID_GENERATOR", sequenceName = "GRANT_CALL_EVALUATION_PANEL_ID_GENERATOR", allocationSize = 1)
	private Integer grantCallEvaluationPanelId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_EVAL_PANEL_FK1"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private GrantCall grantCall;

	@Column(name = "MAP_ID")
	private Integer mapId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_EVAL_PANEL_FK2"), name = "MAP_ID", referencedColumnName = "MAP_ID", insertable = false, updatable = false)
	private WorkflowMap workflowMap;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name="IS_MAIN_PANEL")
	private String isMainPanel;

	public Integer getGrantCallEvaluationPanelId() {
		return grantCallEvaluationPanelId;
	}

	public void setGrantCallEvaluationPanelId(Integer grantCallEvaluationPanelId) {
		this.grantCallEvaluationPanelId = grantCallEvaluationPanelId;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
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

	public String getIsMainPanel() {
		return isMainPanel;
	}

	public void setIsMainPanel(String isMainPanel) {
		this.isMainPanel = isMainPanel;
	}

}

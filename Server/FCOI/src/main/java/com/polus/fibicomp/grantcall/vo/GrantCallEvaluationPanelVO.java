package com.polus.fibicomp.grantcall.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.workflow.pojo.WorkflowMap;

public class GrantCallEvaluationPanelVO {

	private Integer grantCallId;

	private String message;

	private String updateUser;

	private List<WorkflowMap> workflowMaps;

	private Integer grantCallEvaluationPanelId;

	private GrantCallEvaluationPanel grantCallEvaluationPanel;

	private List<GrantCallEvaluationPanel> grantCallEvaluationPanels;	

	private String mapType;

	public GrantCallEvaluationPanelVO() {
		grantCallEvaluationPanel = new GrantCallEvaluationPanel();
		grantCallEvaluationPanels = new ArrayList<GrantCallEvaluationPanel>();
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<WorkflowMap> getWorkflowMaps() {
		return workflowMaps;
	}

	public void setWorkflowMaps(List<WorkflowMap> workflowMaps) {
		this.workflowMaps = workflowMaps;
	}

	public Integer getGrantCallEvaluationPanelId() {
		return grantCallEvaluationPanelId;
	}

	public void setGrantCallEvaluationPanelId(Integer grantCallEvaluationPanelId) {
		this.grantCallEvaluationPanelId = grantCallEvaluationPanelId;
	}

	public GrantCallEvaluationPanel getGrantCallEvaluationPanel() {
		return grantCallEvaluationPanel;
	}

	public void setGrantCallEvaluationPanel(GrantCallEvaluationPanel grantCallEvaluationPanel) {
		this.grantCallEvaluationPanel = grantCallEvaluationPanel;
	}

	public List<GrantCallEvaluationPanel> getGrantCallEvaluationPanels() {
		return grantCallEvaluationPanels;
	}

	public void setGrantCallEvaluationPanels(List<GrantCallEvaluationPanel> grantCallEvaluationPanels) {
		this.grantCallEvaluationPanels = grantCallEvaluationPanels;
	}

	public String getMapType() {
		return mapType;
	}

	public void setMapType(String mapType) {
		this.mapType = mapType;
	}

}

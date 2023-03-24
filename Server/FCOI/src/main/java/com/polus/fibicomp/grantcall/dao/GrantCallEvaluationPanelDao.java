package com.polus.fibicomp.grantcall.dao;

import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.workflow.pojo.WorkflowMap;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;

@Transactional
@Service
public interface GrantCallEvaluationPanelDao {

	/**
	 * This method is used to fetch all Evaluation Panels .
	 * @param mapId
	 * @param mapType
	 * @return A list of Evaluation Panel.
	 */
	public List<WorkflowMap> fetchAllEvaluationPanels(Integer mapId, String mapType);

	/**
	 * This method used to fetch Evaluation Panel ByGrantCall by id
	 * @param mapId
	 * @param grantCallId
	 * @return details of grant call Evaluation panels
	 */
	public List<GrantCallEvaluationPanel> fetchEvaluationPanelByGrantCallId(Integer grantCallId);

	/**
	 * This method is used to save and update grantCall Evaluation Panel;
	 * 
	 * @param GrantCallEvaluationPanel - grantCallEvaluationPanel
	 */
	public GrantCallEvaluationPanel saveOrUpdateGrantCallEvaluationPanel(GrantCallEvaluationPanel grantCallEvaluationPanel);

	/**
	 * This method is used to delete GrantCall based on id.
	 * 
	 * @param grantCallEvaluationPanelId - Id of the GrantCall Evaluation Panel.
	 * @return success message.
	 */
	public String deleteGrantCallEvaluationPanel(Integer grantCallEvaluationPanelId);

}

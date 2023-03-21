package com.polus.fibicomp.grantcall.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.vo.GrantCallEvaluationPanelVO;

@Transactional
@Service
public interface GrantCallEvaluationPanelService {


	/**
	 * This method is used for fetch all Evaluation Panels
	 * 
	 * @param vo
	 * @return
	 */
	public String fetchAllEvaluationPanels(GrantCallEvaluationPanelVO vo);

	/**
	 * This method is used to save or update grant call ScoringCriteria.
	 * 
	 * @param vo - Object of GrantCallEvaluationPanelVO class.
	 * @return set of values to figure out details about a grant call
	 *         ScoringCriteria.
	 */
	public String saveOrUpdateGrantCallEvaluationPanel(GrantCallEvaluationPanelVO vo);

	/**
	 * This method is used to delete GrantCall Evaluation Panel based on id.
	 * 
	 * @param GrantCallId - Id of the GrantCall.
	 * @return success message.
	 */
	public String deleteEvaluationPanel(GrantCallEvaluationPanelVO vo);

}

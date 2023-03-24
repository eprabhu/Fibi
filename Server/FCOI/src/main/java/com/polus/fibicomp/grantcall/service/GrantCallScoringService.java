package com.polus.fibicomp.grantcall.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.vo.GrantCallScoringVO;

@Transactional
@Service
public interface GrantCallScoringService {

	/**
	 * This method is used for fetch all Scoring Criteria
	 * 
	 * @param vo
	 * @return
	 */
	public String fetchAllScoringCriteria(GrantCallScoringVO vo);

	/**
	 * This method is used to save or update grant call ScoringCriteria.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return set of values to figure out details about a grant call
	 *         ScoringCriteria.
	 */
	public String saveOrUpdateGrantCallScoringCriteria(GrantCallScoringVO vo);

	/**
	 * This method is used to delete GrantCall Scoring Criteria based on id.
	 * 
	 * @param GrantCallId - Id of the GrantCall.
	 * @return success message.
	 */
	public String deleteGrantCallScoringCriteria(GrantCallScoringVO vo);

}

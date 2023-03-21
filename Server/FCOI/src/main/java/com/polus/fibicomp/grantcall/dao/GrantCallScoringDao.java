package com.polus.fibicomp.grantcall.dao;

import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.ScoringCriteria;

@Transactional
@Service
public interface GrantCallScoringDao {

	/**
	 * This method is used to fetch all ScoringCriteria .
	 * 
	 * @return A list of ScoringCriteria.
	 */
	public List<ScoringCriteria> fetchAllScoringCriteria();

	/**
	 * This method used to fetch Scoring Criterias ByGrantCall by id
	 * 
	 * @param grantCallId
	 * @return details of grant call Scoring Criteria
	 */
	public List<GrantCallScoringCriteria> fetchScoringCriteriaGrantCallId(Integer grantCallId);

	/**
	 * This method is used to save and update grantcall ScoringCriteria
	 * 
	 * @param GrantCallScoringCriteria - grantCallScoringCriteria
	 */
	public GrantCallScoringCriteria saveOrUpdateGrantCallScoringCriteria(GrantCallScoringCriteria grantCallScoringCriteria);

	/**
	 * This method is used to delete GrantCall based on id.
	 * 
	 * @param grantScoringCriteriaId - Id of the GrantCall Scoring Criteria.
	 * @return success message.
	 */
	public String deleteGrantCallScoringCriteria(Integer grantScoringCriteriaId);

}

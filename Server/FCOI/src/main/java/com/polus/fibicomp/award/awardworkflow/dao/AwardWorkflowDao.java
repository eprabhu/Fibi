package com.polus.fibicomp.award.awardworkflow.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;

@Transactional
@Service
public interface AwardWorkflowDao {

	/**
	 * This method is used to getAwardWorkFlowStatusByCode.
	 * @param statusCode - statusCode
	 * @return awardWorkflowStatus object
	 */
	public AwardWorkflowStatus getAwardWorkFlowStatusByCode(String statusCode);

	/**
	 * This method is used to addAlternativeApprover.
	 * @param vo - awardVO object
	 * @return workflow detail id
	 */
	public Integer addAlternativeApprover(AwardVO vo);

}

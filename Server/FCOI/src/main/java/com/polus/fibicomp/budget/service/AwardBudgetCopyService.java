package com.polus.fibicomp.budget.service;

import java.util.List;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;

public interface AwardBudgetCopyService {

	/**
	 * @param vo
	 * @param award
	 * @param awardBudgetHeader
	 * @param serviceRequestTypeCode
	 * @param awardBudgetHeaderDetail
	 * @return
	 */
	public AwardBudgetHeader createAwardBudgetHeader(AwardVO vo, Award award, AwardBudgetHeader awardBudgetHeader,
			String serviceRequestTypeCode, List<AwardBudgetHeader> awardBudgetHeaderDetail);

}

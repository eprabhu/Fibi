package com.polus.fibicomp.award.service;

import com.polus.fibicomp.award.vo.AwardVO;

public interface AwardConcurrentService {

	/**
	 * @param awardVo
	 * @return String response
	 * @throws Exception
	 */
	public String getAwardDetails(AwardVO awardVo) throws Exception;
}

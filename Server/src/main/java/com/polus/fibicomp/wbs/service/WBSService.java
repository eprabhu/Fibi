package com.polus.fibicomp.wbs.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.vo.AwardVO;

@Service
public interface WBSService {

	/**
	 * This method is used to generate the wbs number
	 * @param vo
	 * @return details of award
	 */
	public ResponseEntity<String> generateWBSNumber(AwardVO vo);

}

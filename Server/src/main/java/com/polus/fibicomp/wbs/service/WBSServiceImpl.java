package com.polus.fibicomp.wbs.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.wbs.dao.WBSDao;

@Service
public class WBSServiceImpl implements WBSService{

	protected static Logger logger = LogManager.getLogger(WBSServiceImpl.class.getName());

	@Autowired
	private WBSDao wbsDao;

	@Autowired
	private AwardService awardService;

	@Override
	public ResponseEntity<String> generateWBSNumber(AwardVO vo) {
		String success = wbsDao.generateWBSNumber(vo.getAwardId(), "Y", vo.getBudgetDetailId(), vo.getBudgetCategoryCode());
		if (success.equals("1")) {
			try {
				return new ResponseEntity<>(awardService.getAwardDetails(vo), HttpStatus.OK);
			} catch (Exception e) {
				logger.error("Error occured in generateWBSNumber : {}", e.getMessage());
			}
		} 
		return new ResponseEntity<>(success, HttpStatus.EXPECTATION_FAILED);
	}

}

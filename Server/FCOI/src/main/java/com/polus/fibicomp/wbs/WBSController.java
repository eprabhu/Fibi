package com.polus.fibicomp.wbs;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.wbs.service.WBSService;

@RestController
public class WBSController {

protected static Logger logger = LogManager.getLogger(WBSController.class.getName());

	@Autowired
	private WBSService wbsService;

	@PostMapping(value = "/generateWBSNumber")
	public ResponseEntity<String> generateWBSNumber(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for generateWBSNumber");
		logger.info("awardId : {}", vo.getAwardId());
		return wbsService.generateWBSNumber(vo);
	}

}

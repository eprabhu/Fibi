package com.polus.fibicomp.grantcall.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.grantcall.service.GrantCallScoringService;
import com.polus.fibicomp.grantcall.vo.GrantCallScoringVO;

@RestController
public class GrantCallScoringController {

	protected static Logger logger =  LogManager.getLogger(GrantCallScoringController.class.getName());

	@Autowired
	@Qualifier(value = "grantCallScoringService")
	private GrantCallScoringService grantCallScoringService;

	
	@RequestMapping(value = "/fetchAllScoringCriteria", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllScoringCriteria(@RequestBody GrantCallScoringVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllScoringCriteria");
		return grantCallScoringService.fetchAllScoringCriteria(vo);
	}

	@RequestMapping(value = "/saveOrUpdateGrantCallScoringCriteria", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateGrantCallScoringCriteria(@RequestBody GrantCallScoringVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateGrantCallScoringCriteria");
		logger.info("GrantCallscoringCriteria : {}", vo.getGrantCallScoringCriteria());
		return grantCallScoringService.saveOrUpdateGrantCallScoringCriteria(vo);
	}

	@RequestMapping(value = "/deleteGrantCallScoringCriteria", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallScoringCriteria(@RequestBody GrantCallScoringVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallScoringCriteria");
		return grantCallScoringService.deleteGrantCallScoringCriteria(vo);
	}

}

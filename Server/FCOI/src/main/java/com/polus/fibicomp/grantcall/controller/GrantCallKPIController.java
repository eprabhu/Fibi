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

import com.polus.fibicomp.grantcall.service.GrantCallKPIService;
import com.polus.fibicomp.grantcall.vo.GrantCallKPIVO;

@RestController
public class GrantCallKPIController {

	protected static Logger logger =  LogManager.getLogger(GrantCallKPIController.class.getName());

	@Autowired
	@Qualifier(value = "grantCallKPIService")
	private GrantCallKPIService grantCallKPIService;


	@RequestMapping(value = "/getKPIByGrantCall", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getKPIByGrantCall(@RequestBody GrantCallKPIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getKPIByGrantCall");
		logger.info("grantCallId : {}", vo.getGrantCallId());
		return grantCallKPIService.getKPIByGrantCall(vo);
	}

	@RequestMapping(value = "/saveOrUpdateGrantCallKPI", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveUpdateGrantCallKPI(@RequestBody GrantCallKPIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveUpdateGrantCallKPI");
		return grantCallKPIService.saveUpdateGrantCallKPI(vo);
	}

	@RequestMapping(value = "/deleteGrantCallKPI", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallKPI(@RequestBody GrantCallKPIVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallKPI");
		return grantCallKPIService.deleteGrantCallKPI(vo);
	}

}

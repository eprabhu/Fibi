package com.polus.fibicomp.externalsystemintegration.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.externalsystemintegration.service.SystemIntegrationService;

@RestController
public class SystemIntegrationController {
	
	protected static Logger logger = LogManager.getLogger(SystemIntegrationController.class.getName());

	
	@Autowired
	@Qualifier(value = "systemIntegrationService")
	private SystemIntegrationService systemIntegrationService;
	
	@RequestMapping(value = "/getExternalApiInfo", method = RequestMethod.GET)
	public String getExternalApiInfo() {
		return systemIntegrationService.getExternalApiInfo();
	}
}

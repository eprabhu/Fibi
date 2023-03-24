package com.polus.fibicomp.person.delegation.delegationController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.person.delegation.delegationService.DelegationService;
import com.polus.fibicomp.person.delegation.vo.DelegationVO;

@RestController
public class DelegationController {

	protected static Logger logger = LogManager.getLogger(DelegationController.class.getName());

	@Autowired
	public DelegationService delegationService;

	@PostMapping(value = "/saveOrUpdateDeligation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateDeligation(@RequestBody DelegationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for saveOrUpdateDeligation");
		return delegationService.saveOrUpdateDeligation(vo);
	}

	@GetMapping(value = "/loadDelegationByPersonId")
	public String getDelegationByPersonId(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for loadDelegationByPersonId");
		return delegationService.loadDelegationByPersonId(request.getHeader("personId"));
	}

	@PostMapping(value = "/updateDeligationStatus", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateDeligationStatus(@RequestBody DelegationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for updateDeligationStatus");
		return delegationService.updateDeligationStatus(vo);
	}

}

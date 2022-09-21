package com.polus.fibicomp.orcid.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.orcid.dto.OrcidVO;
import com.polus.fibicomp.orcid.service.OrcidService;

@RestController
public class OrcidController {

	protected static Logger logger = LogManager.getLogger(OrcidController.class.getName());

	@Autowired
	@Qualifier(value = "orcidService")
	private OrcidService orcidService;

	@PostMapping(value = "/getPersonOrcidWorks")
	public String getPersonOrcidWorks(@RequestBody OrcidVO vo,HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for getPersonOrcidWorks for {} ",vo.getPersonId());
		return orcidService.getPersonOrcidWorks(vo.getPersonId());
	}

	@PostMapping(value = "/getOrcidWorkById")
	public String getOrcidWorkById(@RequestBody OrcidVO vo,HttpServletRequest request, HttpServletResponse response) throws Exception {
		return orcidService.getOrcidWorkById(vo.getPutCode());
	}

	@PostMapping(value = "/linkPersonOrcidWorksToAward")
	public String linkPersonOrcidWorkToAward(@RequestBody OrcidVO vo,HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for linkPersonOrcidWorksToAward ");
		return orcidService.linkPersonOrcidWorkToAward(vo);
	}

	@PostMapping(value = "/unLinkPersonOrcidWorkFromAward")
	public String unLinkPersonOrcidWorkFromAward(@RequestBody OrcidVO vo,HttpServletRequest request, HttpServletResponse response) throws Exception {
		return orcidService.unLinkPersonOrcidWorkFromAward(vo);
	}

	@PostMapping(value = "/getLinkedOrcidWorksOfAward")
	public String getLinkedOrcidWorksOfAward(@RequestBody OrcidVO vo,HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for getPersonOrcidWorks for {} ",vo.getAwardId());
		return orcidService.getLinkedOrcidWorksOfAward(vo);
	}

}

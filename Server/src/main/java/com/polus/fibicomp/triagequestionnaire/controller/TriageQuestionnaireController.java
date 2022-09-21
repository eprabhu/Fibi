package com.polus.fibicomp.triagequestionnaire.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.triagequestionnaire.service.TriageQuestionnaireService;
import com.polus.fibicomp.triagequestionnaire.vo.TriageQuestionnaireVo;

@RestController
public class TriageQuestionnaireController {

	protected static Logger logger = LogManager.getLogger(TriageQuestionnaireController.class.getName());

	@Autowired
	private TriageQuestionnaireService triageQuestionnaireService;

	@PostMapping(value = "/createTriageHeader")
	public String createTriageHeader(@RequestBody TriageQuestionnaireVo vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for createTriageHeader");
		return triageQuestionnaireService.createTriageHeader(vo);
	}

	@PostMapping(value = "/evaluateTriageQuestionnaire")
	public String evaluateTriageQuestionnaire(@RequestBody TriageQuestionnaireVo vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for evaluateTriageQuestionnaire");
		return triageQuestionnaireService.evaluateTriageQuestionnaire(vo);
	}
}

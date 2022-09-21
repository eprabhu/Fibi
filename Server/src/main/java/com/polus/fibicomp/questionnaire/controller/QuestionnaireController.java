package com.polus.fibicomp.questionnaire.controller;

import java.io.ByteArrayInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;

@RestController
public class QuestionnaireController {

	protected static Logger logger = LogManager.getLogger(QuestionnaireController.class.getName());

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private CommonDao commonDao;

	@PostMapping(value = "/getApplicableQuestionnaire")
	public ResponseEntity<String> getApplicableQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for getApplicableQuestionnaire");
		if (questionnaireDataBus != null) {
			questionnaireDataBus = questionnaireService.getApplicableQuestionnaire(questionnaireDataBus);
		}
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireDataBus), HttpStatus.OK);
	}

	@PostMapping(value = "/getQuestionnaire")
	public ResponseEntity<String> getQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for getQuestionnaire");
		if (questionnaireDataBus != null) {
			questionnaireDataBus = questionnaireService.getQuestionnaireDetails(questionnaireDataBus);
		}
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireDataBus), HttpStatus.OK);
	}

	@PostMapping(value = "/saveQuestionnaire")
	public ResponseEntity<String> saveQuestionnaire(MultipartHttpServletRequest request, HttpServletResponse reponse) {
		logger.info("Requesting for saveQuestionnaire");
		ObjectMapper mapper = new ObjectMapper();
		QuestionnaireDataBus questionnaireDataBus = null;
		String formDataJson = request.getParameter("formDataJson");
		try {
		questionnaireDataBus = mapper.readValue(formDataJson, QuestionnaireDataBus.class);
		} catch (Exception e) {
			logger.error("Exception in saveQuestionnaire : {}", e.getMessage());
		}
		return questionnaireService.saveQuestionnaireAnswers(questionnaireDataBus, request);
	}

	@PostMapping(value = "/showAllQuestionnaire")
	public ResponseEntity<String> showAllQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for showAllQuestionnaire");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.showAllQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@GetMapping(value = "/createQuestionnaire")
	public ResponseEntity<String> modifyQuestionnaire(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createQuestionnaire");
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.createQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/configureQuestionnaire")
	public ResponseEntity<String> configureQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for configureQuestionnaire");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.configureQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/editQuestionnaire")
	public ResponseEntity<String> editQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for editQuestionnaire");
		logger.info("questionnaireId : {}", questionnaireDataBus.getQuestionnaireId());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.editQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/downloadQuesAttachment")
	public ResponseEntity<byte[]> downloadAttachment(HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for downloadQuesAttachment");
		logger.info("attachmentId : {}",questionnaireDataBus.getQuestionnaireAnsAttachmentId());
		return questionnaireService.downloadAttachments(questionnaireDataBus, response);
	}

	@PostMapping(value = "/getBusinessRuleForQuestionnaire")
	public ResponseEntity<String> getBusinessRuleForQuestionnaire(HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for getBusinessRuleForQuestionnaire");
		logger.info("moduleItemCode : {}",questionnaireDataBus.getModuleItemCode());
		logger.info("moduleSubItemCode : {}",questionnaireDataBus.getModuleSubItemCode());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.getBusinessRuleForQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/printQuestionnaire", produces = MediaType.APPLICATION_PDF_VALUE)
	public ResponseEntity<InputStreamResource> printQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for printQuestionnaire");
		ByteArrayInputStream bis = questionnaireService.printQuestionnaire(questionnaireDataBus);
		HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Disposition", "inline; filename=QuestionnaireSummary.pdf");
		return ResponseEntity.ok().headers(headers).contentType(MediaType.APPLICATION_PDF).body(new InputStreamResource(bis));
	}

	@PostMapping(value = "/activateQuestionnaire")
	public ResponseEntity<String> activateQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for activateQuestionnaire");
		logger.info("questionnarireId : {}", questionnaireDataBus.getQuestionnaireId());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.activeDeactivateQuestionnaire(questionnaireDataBus,true)), HttpStatus.OK);
	}

	@PostMapping(value = "/deactivateQuestionnaire")
	public ResponseEntity<String> deactivateQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for deactivateQuestionnaire");
		logger.info("questionnarireId : {}", questionnaireDataBus.getQuestionnaireId());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.activeDeactivateQuestionnaire(questionnaireDataBus,false)), HttpStatus.OK);
	}

	@PostMapping(value = "/copyQuestionnaire")
	public ResponseEntity<String> copyQuestionnaire(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for copyQuestionnaire");
		logger.info("questionnaireId: {}", questionnaireDataBus.getQuestionnaireId());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.copyQuestionnaire(questionnaireDataBus)), HttpStatus.OK);
	}

	@GetMapping(value = "/getModuleList")
	public ResponseEntity<String> getModuleList(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getModuleList");
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.getModuleList(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/updateQuestionnaireSortOrder")
	public ResponseEntity<String> updateQuestionnaireSortOrder(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for updateQuestionnaireSortOrder");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.updateQuestionnaireUsage(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/getQuestionnaireListByModule")
	public ResponseEntity<String> getQuestionnaireListByModule(HttpServletRequest request, HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for getQuestionnaireListByModule");
		logger.info("moduleItemCode : {}", questionnaireDataBus.getModuleItemCode());
		logger.info("moduleSubItemCode : {}", questionnaireDataBus.getModuleSubItemCode());
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireService.getQuestionnaireListByModule(questionnaireDataBus)), HttpStatus.OK);
	}

	@PostMapping(value = "/saveQuestionnaireForWaf")
	public ResponseEntity<String> saveQuestionnaireForWaf(HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) throws Exception {
		logger.info("Requesting for saveQuestionnaireForWaf");
		MultipartHttpServletRequest request = null;
		return questionnaireService.saveQuestionnaireAnswers(questionnaireDataBus, request);
	}

	@PostMapping(value = "/ruleEvaluateQuestionnaire")
	public String ruleEvaluateQuestionnaire(HttpServletResponse response, @RequestBody QuestionnaireDataBus questionnaireDataBus) {
		logger.info("Requesting for ruleEvaluateQuestionnaire");
		return commonDao.convertObjectToJSON(questionnaireService.ruleEvaluateQuestionnaire(questionnaireDataBus));
	}

}

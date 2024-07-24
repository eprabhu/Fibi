package com.polus.integration.proposal.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.integration.proposal.service.ProposalIntegrationService;


@RestController
public class ProposalIntegrationController {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationController.class.getName());

	@Autowired
	private ProposalIntegrationService proposalIntegrationService;

    @PostMapping("/getIntegrationQuestionnaire")
    public ResponseEntity<String> getIntegrationQuestionnaire(@RequestBody QuestionnaireDataBus questionnaireDataBus) {
    	logger.info("Request for getIntegrationQuestionnaire {}");
    	return new ResponseEntity<>(proposalIntegrationService.getQuestionnaire(questionnaireDataBus), HttpStatus.OK);
    }

}

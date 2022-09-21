package com.polus.fibicomp.rule.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.questionnaire.service.QuestionnaireServiceImpl;
import com.polus.fibicomp.rule.dto.WorkFlowDataBus;
import com.polus.fibicomp.rule.service.WorkFlowService;


@Controller
public class WorkFlowController {

	protected static Logger logger = LogManager.getLogger(QuestionnaireServiceImpl.class.getName());

	@Autowired 
	WorkFlowService workFlowService;

	@RequestMapping(value = "/getBusinessRule", method = RequestMethod.GET)
	public ResponseEntity<String> getBusinessRule(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		WorkFlowDataBus businessRuleDataBus = new WorkFlowDataBus();
		businessRuleDataBus = workFlowService.getRuleType(businessRuleDataBus);
		String responseData = mapper.writeValueAsString(businessRuleDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getUnitList", method = RequestMethod.GET)
	public ResponseEntity<String> getUnitList(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		WorkFlowDataBus businessRuleDataBus = new WorkFlowDataBus();
		businessRuleDataBus = workFlowService.getUnitList(businessRuleDataBus);
		String responseData = mapper.writeValueAsString(businessRuleDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getRuleList", method = RequestMethod.POST)
	public ResponseEntity<String> getRuleList(@RequestBody WorkFlowDataBus workFlowDataBus, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getRuleLists(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getRuleDetails", method = RequestMethod.POST)
	public ResponseEntity<String> getRuleDetails(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getRuleDetails(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/inActivateRule", method = RequestMethod.POST)
	public ResponseEntity<String> inActivateRule(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.inActivateRule(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/insertBusinessRule", method = RequestMethod.POST)
	public ResponseEntity<String> insertRule(@RequestBody WorkFlowDataBus workFlowDataBus, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.insertRule(workFlowDataBus);
 		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/updateBusinessRule", method = RequestMethod.POST)
	public ResponseEntity<String> updateRule(@RequestBody WorkFlowDataBus workFlowDataBus, HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.updateRule(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getBusinessRuleById", method = RequestMethod.POST)
	public ResponseEntity<String> getBusinessRuleById(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getBusinessRuleById(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getByRuleVariable", method = RequestMethod.POST)
	public ResponseEntity<String> getByRuleVariable(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getByRuleVariable(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getQuestionsById", method = RequestMethod.POST)
	public ResponseEntity<String> getQuestionsById(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getQuestionsById(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getNotificationList", method = RequestMethod.GET)
	public ResponseEntity<String> getNotificationList(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		WorkFlowDataBus businessRuleDataBus = new WorkFlowDataBus();
		businessRuleDataBus = workFlowService.getNotificationList(businessRuleDataBus);
		String responseData = mapper.writeValueAsString(businessRuleDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/updateRuleEvaluationOrder", method = RequestMethod.POST)
	public ResponseEntity<String> updateRuleEvaluationOrder(@RequestBody WorkFlowDataBus workFlowDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.updateRuleEvaluationOrder(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@RequestMapping(value = "/getAllRuleList", method = RequestMethod.GET)
	public ResponseEntity<String> getAllRuleList(HttpServletRequest request,
			HttpServletResponse response) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		WorkFlowDataBus workFlowDataBus = new WorkFlowDataBus();
		workFlowDataBus = workFlowService.getAllRuleLists(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<String>(responseData, status);
	}

	@PostMapping(value = "/getFunctionParameters")
	public ResponseEntity<String> getFunctionParameters(@RequestBody WorkFlowDataBus workFlowDataBus) throws Exception {
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getFunctionParameters(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<>(responseData, status);
	}

	@PostMapping(value = "/getActiveQuestions")
	public ResponseEntity<String> getActiveQuestions(@RequestBody WorkFlowDataBus workFlowDataBus) throws Exception {
		logger.info("Request for getActiveQuestions : {}", workFlowDataBus.getModuleCode());
		HttpStatus status = HttpStatus.OK;
		ObjectMapper mapper = new ObjectMapper();
		workFlowDataBus = workFlowService.getActiveQuestions(workFlowDataBus);
		String responseData = mapper.writeValueAsString(workFlowDataBus);
		return new ResponseEntity<>(responseData, status);
	}

	@DeleteMapping("/businessRule/{ruleId}")
	public ResponseEntity<String> deleteBusinessRule(@PathVariable("ruleId") Integer ruleId) throws Exception {
		boolean status = workFlowService.deleteBusinessRule(ruleId);
		if (status)
			return new ResponseEntity<>("Deleted successfully", HttpStatus.OK);
		else
			return new ResponseEntity<>("Unable to delete", HttpStatus.INTERNAL_SERVER_ERROR);
	}
}

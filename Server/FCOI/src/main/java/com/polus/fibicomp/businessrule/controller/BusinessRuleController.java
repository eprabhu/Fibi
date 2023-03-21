package com.polus.fibicomp.businessrule.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.awardworkflow.service.AwardWorkflowService;
import com.polus.fibicomp.award.service.AwardConcurrentService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.workflow.pojo.Workflow;

import io.jsonwebtoken.Claims;

@RestController
public class BusinessRuleController {

	protected static Logger logger = LogManager.getLogger(BusinessRuleController.class.getName());

	@Autowired
	@Qualifier(value = "businessRuleService")
	private BusinessRuleService businessRuleService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardConcurrentService awardConcurrentService;

	@Autowired
	private AwardWorkflowService awardWorkflowService;

	private static final String MODULE_CODE = "moduleCode : {}";
	private static final String UPDATE_USER = "updateUser : {}";
	private static final String MODULE_ITEM_KEY = "moduleItemKey : {}";
	private static final String SUB_MODULE_ITEM_KEY = "subModuleItemKey : {}";
	private static final String LOGIN_PERSON_ID = "loginPersonId : {}";
	private static final String SUB_MODULE_CODE = "subModuleCode : {}";

	@PostMapping(value = "/buildWorkFlow", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Integer buildWorkFlow(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for buildWorkFlow");
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		logger.info(SUB_MODULE_CODE, evaluateValidationRuleVO.getSubModuleCode());
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(LOGIN_PERSON_ID, evaluateValidationRuleVO.getLogginPersonId());
		logger.info(UPDATE_USER, evaluateValidationRuleVO.getUpdateUser());
		return businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
	}

	@PostMapping(value = "/evaluateValidationRule", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String evaluateValidationRule(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for evaluateValidationRule");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		evaluateValidationRuleVO.setLogginPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
		evaluateValidationRuleVO.setUpdateUser(claims.getSubject());
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		logger.info(SUB_MODULE_CODE, evaluateValidationRuleVO.getSubModuleCode());
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(LOGIN_PERSON_ID, evaluateValidationRuleVO.getLogginPersonId());
		logger.info(UPDATE_USER, evaluateValidationRuleVO.getUpdateUser());
		logger.info(SUB_MODULE_ITEM_KEY, evaluateValidationRuleVO.getSubModuleItemKey());
		return businessRuleService.evaluateValidationRule(evaluateValidationRuleVO);
	}

	@PostMapping(value = "/evaluateNotificationRule", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String evaluateNotificationRule(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for evaluateNotificationRule");
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		logger.info(SUB_MODULE_CODE, evaluateValidationRuleVO.getSubModuleCode());
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(LOGIN_PERSON_ID, evaluateValidationRuleVO.getLogginPersonId());
		logger.info(UPDATE_USER, evaluateValidationRuleVO.getUpdateUser());
		logger.info(SUB_MODULE_ITEM_KEY, evaluateValidationRuleVO.getSubModuleItemKey());
		return businessRuleService.evaluateNotificationRule(evaluateValidationRuleVO);
	}

	@PostMapping(value = "/ruleEvaluate", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String ruleEvaluate(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for ruleEvaluate");
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		logger.info(SUB_MODULE_CODE, evaluateValidationRuleVO.getSubModuleCode());
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(LOGIN_PERSON_ID, evaluateValidationRuleVO.getLogginPersonId());
		logger.info(UPDATE_USER, evaluateValidationRuleVO.getUpdateUser());
		logger.info("ruleId : {}", evaluateValidationRuleVO.getRuleId());
		return businessRuleService.ruleEvaluate(evaluateValidationRuleVO);
	}

	@PostMapping(value = "/getWorkFlowRouteLog", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getWorkFlowRouteLog(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getWorkFlowRouteLog");
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		return businessRuleService.getWorkFlowRouteLog(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getModuleCode());
	}

	@PostMapping(value = "/workflowfinalApproval", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String workflowfinalApproval(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for workflowfinalApproval");
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info("logginPersonId : {}", evaluateValidationRuleVO.getLogginPersonId());
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		logger.info(SUB_MODULE_CODE, evaluateValidationRuleVO.getSubModuleCode());
		logger.info(SUB_MODULE_ITEM_KEY, evaluateValidationRuleVO.getSubModuleItemKey());
		return businessRuleService.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getSubModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey());
	}

	@PostMapping(value = "/approveOrRejectWorkflow", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveProposal(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, @RequestParam("moduleCode") String moduleCode, @RequestParam("subModuleCode") String subModuleCode) throws Exception {
		logger.info("Requesting for approveOrRejectWorkflow");
		logger.info(MODULE_CODE, moduleCode);
		logger.info(SUB_MODULE_CODE, subModuleCode);
		if (moduleCode.equals(Constants.AWARD_MODULE_CODE.toString()) && subModuleCode.equals(Constants.AWARD_SUBMODULE_CODE.toString())) {
			AwardVO vo = businessRuleService.awardWorkflowApproval(files, formDataJson, moduleCode);
			vo.setIsManpowerIntegrationRequired(true);
			return awardConcurrentService.getAwardDetails(vo);
		} else {
			return businessRuleService.approveOrRejectWorkflow(files, formDataJson, moduleCode, subModuleCode);
		}
	}

	@PostMapping(value = "/getWorkFlow", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Workflow getWorkFlow(@RequestBody EvaluateValidationRuleVO evaluateValidationRuleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getWorkFlowRouteLog ");
		logger.info(MODULE_ITEM_KEY, evaluateValidationRuleVO.getModuleItemKey());
		logger.info(MODULE_CODE, evaluateValidationRuleVO.getModuleCode());
		return businessRuleService.getWorkFlow(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getModuleCode());
	}

	@GetMapping(value = "/downloadWorkflowsAttachments")
	public ResponseEntity<byte[]> downloadWorkflowsAttachments(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadNegotiationAttachment");
		logger.info("negotiationsAttachmentId : {}", attachmentId);
		Integer workflowAttachmentId = Integer.parseInt(attachmentId);
		return businessRuleService.downloadWorkflowsAttachments(workflowAttachmentId);
	}

	@PostMapping(value = "/approveOrRejectWorkflowForWaf", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveProposalForWaf(@RequestParam(value = "files", required = false) MultipartFile file, @RequestParam("formDataJson") String formDataJson, @RequestParam("moduleCode") String moduleCode) {
		logger.info("Requesting for approveOrRejectWorkflow");
		logger.info("moduleCode : " + moduleCode);
		return businessRuleService.approveOrRejectWorkflowForWaf(file, formDataJson, moduleCode);
	}

	@PostMapping(value = "/approveOrRejectAwardWorkflow", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveOrRejectAwardForWaf(@RequestBody AwardVO awardVO, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for approveOrRejectAwardWorkflow");
		if (Boolean.TRUE.equals(awardVO.getIsLastRequest())) {
			awardVO = businessRuleService.approveOrRejectAwardWorkflow(awardVO);
			awardVO.setIsManpowerIntegrationRequired(true);
			return awardConcurrentService.getAwardDetails(awardVO);
		} else {
			return businessRuleService.approveOrRejectAwardWorkflowForWaf(awardVO);
		}
	}

}

package com.polus.fibicomp.servicerequest.controller;


import java.security.Principal;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.service.ServiceRequestService;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;

@RestController
public class ServiceRequestController {

	protected static Logger logger = LogManager.getLogger(ServiceRequestController.class.getName());

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private ServiceRequestService serviceRequestService;

	@PostMapping(value = "/deleteServiceRequestAttachment")
	public String deleteServiceRequestAttachment(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteServiceRequestAttachment");
		return serviceRequestService.deleteServiceRequestAttachment(vo);
	}

	@GetMapping(value = "/downloadServiceRequestAttachment")
	public ResponseEntity<byte[]> downloadServiceRequestAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadServiceRequestAttachment");
		logger.info("attachmentId : {}", attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return serviceRequestService.downloadServiceRequestAttachment(attachmentid);
	}

	@PostMapping(value = "/deleteSRAttachment")
	public String deleteSRAttachment(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteServiceRequestAttachment From Award");
		return serviceRequestService.deleteServiceRequestAttachmentFromAward(vo);
	}

	@GetMapping(value = "/getServiceRequestTypes", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getServiceRequestTypes(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getServiceRequestTypes");
		return serviceRequestService.getServiceRequestTypes();
	}

/*	@PostMapping(value = "/canCreateServiceRequest")
	public String canCreateServiceRequest(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for canCreateServiceRequest");
		return serviceRequestService.canCreateServiceRequest(vo);
	}

	@PostMapping(value = "/addServiceRequestAttachmentForWaf")
	public String addServiceRequestAttachmentForWaf(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addServiceRequestAttachmentForWaf");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		logger.info("loginPersonId : {}", vo.getPersonId());
		logger.info("unitNumber : {}", vo.getLoginPersonUnitNumber());
		logger.info("update User : {}", vo.getUpdateUser());
		return serviceRequestService.addServiceRequestAttachmentForWaf(vo);
	}*/

	@PostMapping(value = "/addSRCommentFromAward")
	public String addSRCommentFromAward(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletRequest request, Principal principal) {
		logger.info("Requesting for addSRCommentFromAward");
		return serviceRequestService.addServiceRequestCommentAndAttachment(files, formDataJson, request);
	}

/*	@PostMapping(value = "/addSRCommentFromAwardForWaf")
	public String addSRCommentFromAwardForWaf(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addSRCommentAndAttachmentForWaf");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		logger.info("loginPersonId : {}", vo.getPersonId());
		logger.info("unitNumber : {}", vo.getLoginPersonUnitNumber());
		logger.info("update User : {}", vo.getUpdateUser());
		return serviceRequestService.addSRCommentAndAttachmentForWaf(vo);
	}*/

	@GetMapping(value = "/getServiceRequestTypeBasedOnModule/{moduleCode}", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getServiceRequestTypeBasedOnModule(@PathVariable (value="moduleCode", required = true) final Integer moduleCode) {
		logger.info("Requesting for getServiceRequestTypeBasedOnModule");
		return serviceRequestService.getServiceRequestTypesBasedOnModule(moduleCode);
	}

	@PostMapping(value = "/submitServiceRequest")
	public String submitServiceRequest(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws Exception {
		logger.info("Requesting for submitServiceRequest");
		return serviceRequestService.submitServiceRequest(files, formDataJson);
	}

	@GetMapping(value = "/getSRCommentsAndAttachments/{serviceRequestId}")
	public String getSRCommentsAndAttachments(@PathVariable (value="serviceRequestId", required = true) final Integer serviceRequestId) {
		logger.info("Requesting for getSRCommentsAndAttachments");
		return serviceRequestService.getSRCommentsAndAttachments(serviceRequestId);
	}

	@PostMapping(value = "/addServiceRequestCommentAndAttachment")
	public String addServiceRequestCommentAndAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletRequest request) {
		logger.info("Requesting for addServiceRequestCommentAndAttachment");
		return serviceRequestService.addServiceRequestCommentAndAttachment(files, formDataJson, request);
	}

	@GetMapping(value = "/createServiceRequest", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createServiceRequest(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createServiceRequest");
		return serviceRequestService.createServiceRequest();
	}

	@GetMapping(value = "/loadServiceRequestById/{serviceRequestId}")
	public ResponseEntity<String> loadServiceRequestById(@PathVariable (value="serviceRequestId", required = true) final Integer serviceRequestId) {
		logger.info("Requesting for loadServiceRequestById");
		HttpStatus httpStatus = HttpStatus.OK;
		if (!documentAuthorization.isAuthorized(20, serviceRequestId.toString(), AuthenticatedUser.getLoginPersonId())) {
			httpStatus = HttpStatus.FORBIDDEN;
			return new ResponseEntity<>("Not Authorized to view this Service Request", httpStatus);
		}
		return new ResponseEntity<>(serviceRequestService.loadServiceRequestById(serviceRequestId), httpStatus);
	}

	@PostMapping(value = "/saveOrUpdateServiceRequest", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateServiceRequest(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateServiceRequest");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("LoginPersonId : {}", vo.getPersonId());
		return serviceRequestService.saveOrUpdateServiceRequest(vo);
	}

	@PostMapping(value = "/saveServiceRequestWatcher")
	public String saveServiceRequestWatcher(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveServiceRequestWatcher");
		return serviceRequestService.saveServiceRequestWatcher(vo);
	}

	@DeleteMapping(value = "/deleteServiceRequestWatcher/{watcherId}")
	public String deleteServiceRequestWatcher(@PathVariable final Integer watcherId) {
		logger.info("Requesting for deleteServiceRequestWatcher");
		return serviceRequestService.deleteServiceRequestWatcher(watcherId);
	}

	@PostMapping(value = "/assignReviewer")
	public String assignReviewer(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws Exception {
		logger.info("Requesting for assignReviewer");
		return serviceRequestService.assignReviewer(files, formDataJson);
	}

	@PostMapping(value = "/resolveServiceRequest")
	public String resolveServiceRequest(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws Exception {
		logger.info("Requesting for resolveServiceRequest");
		return serviceRequestService.resolveServiceRequest(files, formDataJson);
	}

	@PostMapping(value = "/updateSRReporter")
	public String updateSRReporter(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateSRReporter");
		return serviceRequestService.updateSRReporter(vo);
	}

	@GetMapping(value = "/loadServiceRequestHistory/{serviceRequestId}")
	public String loadServiceRequestHistory(@PathVariable (value="serviceRequestId", required = true) final Integer serviceRequestId) {
		logger.info("Requesting for loadServiceRequestHistory");
		return serviceRequestService.loadServiceRequestHistory(serviceRequestId);
	}

	@GetMapping(value = "/loadModuleDetail/{moduleCode}/{moduleItemKey}")
	public ModuleDetails loadModuleDetail(@PathVariable (value="moduleCode", required = true) final Integer moduleCode, @PathVariable (value="moduleItemKey", required = true) final Integer moduleItemKey) {
		logger.info("Request for loadModuleDetail");
		logger.info("moduleItemKey {}", moduleItemKey);
		return serviceRequestService.prepareModuleDetails(moduleCode, moduleItemKey);
	}

	@GetMapping(value = "/loadSRInitialData", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ServiceRequestVO loadInitialData(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadInitialData");
		return serviceRequestService.loadInitialData();
	}
}

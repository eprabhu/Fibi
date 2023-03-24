package com.polus.fibicomp.customdataelement.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.common.dto.ResponseData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.customdataelement.service.CustomDataElementService;
import com.polus.fibicomp.customdataelement.vo.CustomDataElementVO;

import io.jsonwebtoken.Claims;

import java.util.List;
import java.util.Map;

@RestController
public class CustomDataElementController {

	protected static Logger logger = LogManager.getLogger(CustomDataElementController.class.getName());

	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	private CommonService commonService;

	@PostMapping(value = "/configureCustomElement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String configureCustomElement(@RequestBody CustomDataElementVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for configureCustomElement");
		return customDataElementService.configureCustomElement(vo);
	}

	@PostMapping(value = "/fetchAllCustomElement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllCustomElement(@RequestBody CustomDataElementVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for fetchAllCustomElement");
		return customDataElementService.fetchAllCustomElement(vo);
	}

	@PostMapping(value = "/fetchCustomElementById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchCustomElementById(@RequestBody CustomDataElementVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for fetchCustomElementById");
		logger.info("customDataElementId : {}", vo.getCustomDataElementId());
		return customDataElementService.fetchCustomElementById(vo);
	}

	@PostMapping(value = "/activeDeactivateCustomElementById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String activeDeactivateCustomElementById(@RequestBody CustomDataElementVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for activeDeactivateCustomElementById");
		logger.info("customDataElementId : {}", vo.getCustomDataElementId());
		return customDataElementService.activeDeactivateCustomElementById(vo);
	}

	@GetMapping(value = "/getCustomElementDataTypes", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getCustomElementDataTypes(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCustomElementDataTypes");
		return customDataElementService.getCustomElementDataTypes();
	}

	@PostMapping(value = "/saveCustomResponse", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveCustomResponse(@RequestBody CustomDataElementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for saveCustomResponse");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		vo.setUpdateUser(claims.getSubject());
		return customDataElementService.saveCustomResponse(vo);
	}

	@PostMapping(value = "/getApplicatbleCustomElement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getApplicatbleCustomElement(@RequestBody CustomDataElementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for getApplicatbleCustomElement");
		logger.info("moduleCode : {}", vo.getModuleCode());
		logger.info("moduleItemKey : {}", vo.getModuleItemKey());
		return customDataElementService.getApplicableCustomElement(vo);
	}

	@GetMapping(value = "/getApplicableModules", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getApplicableModules(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCustomElementDataTypes");
		return customDataElementService.getApplicableModules();
	}

	@PostMapping(value = "/getSystemLookupByCustomType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getSystemLookupByCustomType(@RequestBody CustomDataElementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for getSystemLookupByCustomType");
		logger.info("dataTypeCode : {}", vo.getDataTypeCode());
		return customDataElementService.getSystemLookupByCustomType(vo);
	}

	@GetMapping(value = "/customElements/{moduleCode}", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<ResponseData> getCustomElements(@PathVariable("moduleCode") Integer moduleCode) {
		logger.info("getCustomElements moduleCode: {}", moduleCode);
		return customDataElementService.getCustomElements(moduleCode);
	}

	@PutMapping("/customElements/{moduleCode}")
	public ResponseEntity<ResponseData> saveCustomElementOrder(@RequestBody List<Map<String, Integer>> cusEleOrderList, @PathVariable("moduleCode") Integer moduleCode) {
		logger.info("saveCustomElementOrder with module code{}", moduleCode);
		return customDataElementService.saveCustomElementOrder(cusEleOrderList, moduleCode);
	}

	@PatchMapping("/customElements/{elementId}/{moduleCode}")
	public ResponseEntity<ResponseData> updateCustomElementRequired(@PathVariable("elementId") Integer elementId,
																	@PathVariable("moduleCode") Integer moduleCode) {
		logger.info("updateCustomElementRequired@ moduleCode: {} customElementId: {}", moduleCode, elementId);
		return customDataElementService.updateCustomElementRequired(elementId, moduleCode);
	}

	@GetMapping(value = "/customElements/modules", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<ResponseData> getCusElementModules() {
		logger.info("getCusElementModules ");
		return customDataElementService.getCusElementModules();
	}
}

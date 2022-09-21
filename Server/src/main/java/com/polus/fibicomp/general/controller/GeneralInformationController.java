package com.polus.fibicomp.general.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.polus.fibicomp.common.dto.ResponseData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.general.service.GeneralInformationService;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.SponsorMaintenanceVO;

@RestController
public class GeneralInformationController {

	protected static Logger logger = LogManager.getLogger(GeneralInformationController.class.getName());

	@Autowired
	@Qualifier(value = "commonDao")
	private CommonDao commonDao;

	@Autowired
	@Qualifier(value = "commonService")
	private CommonService commonService;

	@Autowired
	@Qualifier(value = "generalInformationService")
	private GeneralInformationService generalInformationService;

	@PostMapping(value = "/getUnitName")
	public String requestResearchSummaryData(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getUnitName");
		logger.info("unitNumber : {} ", vo.getUnitNumber());
		return commonDao.convertObjectToJSON(commonDao.getUnitName(vo.getUnitNumber()));
	}

	@PostMapping(value = "/maintainSponsor")
	public String maintainSponsor(@RequestBody SponsorMaintenanceVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for maintainSponsor");
		logger.info("sponsorCode: {} ", vo.getSponsorCode());
		return commonDao.convertObjectToJSON(generalInformationService.fetchSponsorData(vo.getSponsorCode()));
	}

	@PostMapping(value = "/createNewSponsor")
	public String createNewSponsor(HttpServletRequest request) throws Exception {
		logger.info("Requesting for createNewSponsor");
		return commonDao.convertObjectToJSON(generalInformationService.createNewSponsor());
	}

	@PostMapping(value = "/saveSponsor")
	public String saveSponsor(@RequestBody SponsorMaintenanceVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for saveSponsor");
		return commonDao.convertObjectToJSON(generalInformationService.saveSponsor(vo));
	}

	@GetMapping(value = "/fibifaq", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fibifaq(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fibifaq");
		return commonService.fibiFaq();
	}

	@GetMapping(value = "/syncPersonRole", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String syncPersonRole(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for syncPersonRole");
		return generalInformationService.syncPersonRole();
	}

	@PostMapping(value = "/getSponsorName")
	public String getSponsorName(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getSponsorName");
		logger.info("sponsorCode : {}" , vo.getSponsorCode());
		return commonDao.convertObjectToJSON(commonDao.getSponsorName(vo.getSponsorCode()));
	}

	@PostMapping(value = "/getAllSponsors")
	public String getAllSponsors(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getAllSponsors");
		return generalInformationService.getAllSponsors(vo);
	}

	@PostMapping(value = "/getNotificationPlaceholder")
	public String getNotificationPlaceholder(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getNotificationPlaceholder");
		logger.info("moduleCode : {}", vo.getModuleCode());
		return commonService.getNotificationPlaceholder(vo.getModuleCode());
	}

	@PostMapping(value = "/getLookUpDatas")
	public String getLookUpDatas(@Valid @RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getLookUpDatas");
		logger.info("lookUpTable : {}", vo.getLookUpTableName());
		logger.info("lookUpCode : {}", vo.getLookUpTableColumnName());
		return commonService.getLookUpDatas(vo.getLookUpTableName(), vo.getLookUpTableColumnName());
	}

	@PostMapping(value = "/fetchHelpTexts")
	public String fetchHelpTexts(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchHelpTexts");
		return generalInformationService.fetchHelpText(vo);
	}

	@ResponseStatus(HttpStatus.BAD_REQUEST)
	@ExceptionHandler(MethodArgumentNotValidException.class)
	public Map<String, String> handleValidationExceptions(MethodArgumentNotValidException ex) {
		Map<String, String> errors = new HashMap<>();
		ex.getBindingResult().getAllErrors().forEach(error -> {
			String fieldName = ((FieldError) error).getField();
			String errorMessage = error.getDefaultMessage();
			errors.put(fieldName, errorMessage);
		});
		return errors;
	}

	@PostMapping(value = "/findResearchTypeArea", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<ResearchTypeArea> findResearchTypeArea(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Research Area");
		logger.info("searchString : {}" , vo.getSearchString());
		return commonService.findResearchTypeArea(vo.getSearchString(), vo.getResearchTypeCode());
	}

	@PostMapping(value = "/findResearchTypeSubArea", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<ResearchTypeSubArea> findResearchTypeSubArea(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Research Sub Area");
		logger.info("searchString : {}" , vo.getSearchString());
		logger.info("researchAreaCode : {}" , vo.getResearchTypeAreaCode());
		return commonService.findResearchTypeSubArea(vo.getSearchString(), vo.getResearchTypeAreaCode(), vo.getResearchTypeCode());
	}
	
	@GetMapping(value = {"/getModulesConfiguration","/getModulesConfiguration/{moduleCode}"})
	public String getModulesConfiguration(@PathVariable(value = "moduleCode", required = false) final String moduleCode) {
		logger.info("Request for getModulesConfiguration");
		return generalInformationService.getModulesConfiguration(moduleCode);
	}
	
	@PostMapping(value = "/findRole")
	public List<Role> findRole(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findRole");
		return commonService.findRole(vo.getSearchString());
	}
	
	@PostMapping(value = "/personCertificationMailLog")
	public String personCertificationMailLog(@RequestBody CommonVO commonVO) {
		logger.info("Request for personCertificationMailLog");
		return commonService.personCertificationMailLog(commonVO);
	}

	@PostMapping( "/letterTemplate")
	public ResponseEntity<Object> getAllLetterTemplateTypes(@RequestBody CommonVO vo) {
		return commonService.getAllLetterTemplateTypes(vo);
	}
}

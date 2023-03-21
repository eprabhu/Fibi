package com.polus.fibicomp.manpower.controller;

import java.util.List;
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
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.manpower.vo.ManpowerVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class ManpowerController {

	protected static Logger logger =  LogManager.getLogger(ManpowerController.class.getName());

	@Autowired
	@Qualifier(value = "manpowerService")
	private ManpowerService manpowerService;

	@GetMapping(value = "/fetchAllManpowerLookUpDatas", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllManpowerLookUpDatas(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllManpowerLookUpDatas");
		Integer awardId = Integer.parseInt(request.getHeader("awardId"));
		return manpowerService.fetchAllManpowerLookUpDatas(awardId);
	}

	@PostMapping(value = "/createManpowerPlan", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createManpowerPlan(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createManpowerPlan");
		return manpowerService.createManpowerPlan(vo);
	}

	@PostMapping(value = "/fetchManpowerDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchManpowerDetails(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchManpowerDetails");
		return manpowerService.fetchManpowerDetails(vo);
	}

	@PostMapping(value = "/saveOrUpdateManpowerResource", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateManpowerResource(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateManpowerResource");
		ManpowerVO manpowerVo = manpowerService.saveOrUpdateManpowerResource(vo);
		return manpowerService.updateManpowerDetails(manpowerVo);
	}

	@PostMapping(value = "/saveOrUpdateAwardManpower", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardManpower(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardManpower");
		return manpowerService.saveOrUpdateAwardManpower(vo);
	}

	@PostMapping(value = "/fetchManpowerBaseSalaryDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> fetchManpowerBaseSalaryDetails(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchManpowerBaseSalaryDetails");
		logger.info("awardNumber : {}", vo.getAwardNumber());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("accountNumber : {}", vo.getAccountNumber());
		return manpowerService.fetchManpowerBaseSalaryDetails(vo.getAwardNumber(), vo.getPersonId(), vo.getAccountNumber());
	}

	@PostMapping(value = "/deleteManpowerResource", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteManpowerResource(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteManpowerResource");
		return manpowerService.deleteManpowerResource(vo);
	}

	@PostMapping(value = "/saveOrUpdateManpowerTriggerDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateManpowerTriggerDetail(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateManpowerTriggerDetails");
		return manpowerService.saveOrUpdateManpowerTriggerDetail(vo);
	}

	@PostMapping(value = "/fetchManpowerPayrollDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchManpowerPayrollDetails(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchManpowerPayrollDetails");
		return manpowerService.fetchManpowerPayrollDetails(vo);
	}

	@PostMapping(value = "/findPersonsWithPositionId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<ManpowerPersonSearchResult> getPersonsWithPositionId(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findPersonsWithPositionId");
		logger.info("searchString : {}", vo.getSearchString());
		logger.info("isGraduateStudent : {}", vo.getIsGraduateStudent());
		return manpowerService.getPersonsWithPositionId(vo.getSearchString(), vo.getIsGraduateStudent(), vo.getAwardId(), vo.getManpowerRequestType());
	}

	@PostMapping(value = "/findManpowerJobProfile", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<ManpowerJobProfileType> getManpowerJobProfile(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getManpowerJobProfile");
		logger.info("searchString : {}", vo.getSearchString());
		logger.info("costElementCode : {}", vo.getCostElementCode());
		return manpowerService.getManpowerJobProfile(vo.getSearchString(), vo.getCostElementCode());
	}
	
	@PostMapping(value = "/calculatePlannedSalary", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String calculatePlannedSalary(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for calculatePlannedSalary");
		return manpowerService.calculatePlannedSalary(vo);
	}

	@PostMapping(value = "/overrideActualCommittedAmount", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String overrideActualCommittedAmount(@RequestBody ManpowerVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for overrideActualCommittedAmount");
		logger.info("awardManpowerResourceId : {}", vo.getManpowerResourceId());
		logger.info("actualCommittedAmount : {}", vo.getActualCommittedAmount());
		return manpowerService.overrideActualCommittedAmount(vo);
	}

	@GetMapping(value = "/fetchAwardManpowerForComparison", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAwardManpowerForComparison(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAwardManpowerForComparison");
		Integer awardId = Integer.parseInt(request.getHeader("awardId"));
		return manpowerService.fetchAwardManpowerForComparison(awardId);
	}

}

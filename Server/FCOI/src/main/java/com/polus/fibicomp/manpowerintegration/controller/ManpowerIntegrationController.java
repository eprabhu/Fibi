package com.polus.fibicomp.manpowerintegration.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;


import com.polus.fibicomp.manpowerintegration.scheduler.ManpowerIntegrationSchedulerService;
import com.polus.fibicomp.manpowerintegration.service.ManpowerIntegrationService;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;

@RestController
public class ManpowerIntegrationController {

	protected static Logger logger = LogManager.getLogger(ManpowerIntegrationController.class.getName());

	@Autowired
	private ManpowerIntegrationService manpowerIntegrationService;

	@Autowired
	private ManpowerIntegrationSchedulerService manpowerIntegrationSchedulerService;

	@GetMapping(value = "/getWorkdayLongLeave")
	public void getWorkdayLongLeave(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getWorkdayLongLeave");
		manpowerIntegrationSchedulerService.startWorkdayLongLeave();
	}

	@GetMapping(value = "/getWorkdayTerminations")
	public void getWorkdayTerminations(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getWorkdayTerminations");
		manpowerIntegrationSchedulerService.startWorkdayTerminations();
	}

	@GetMapping(value = "/getCostingAllocationReconciliation")
	public void getCostingAllocationReconciliation(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCostingAllocationReconciliation");
		manpowerIntegrationSchedulerService.startCostingAllocationReconciliation();
	}

	@GetMapping(value = "/getWorkdayJobProfile")
	public void getWorkdayJobProfile(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getWorkdayJobProfile");		
		manpowerIntegrationSchedulerService.getAllJobProfiles();
	}

	@GetMapping(value = "/getManpowerDetails")
	public void getManpowerDetails() {
		logger.info("Requesting for getManpowerDetails after hire");
		manpowerIntegrationSchedulerService.getAllManpowerDetails();
	}

	@GetMapping(value = "/getNationalityDetails")
	public void getNationalityDetails() {
		logger.info("Requesting for getNationalityDetails");
		manpowerIntegrationSchedulerService.getCitizenshipNationalityDetails();
	}

	@GetMapping(value = "/closePosition")
	public void closePosition() {
		logger.info("Requesting for closePosition");
		manpowerIntegrationSchedulerService.closePosition();
	}

	@GetMapping(value = "/assignCostAllocation")
	public void assignCostAllocation() {
		logger.info("Requesting for assignCostAllocation");
		manpowerIntegrationSchedulerService.assignCostAllocation();
	}

	@GetMapping(value = "/exportManpowerDetails")
	public void exportManpowerDetails(HttpServletRequest request) {
		logger.info("Requesting for exportManpowerDetails");
		manpowerIntegrationService.getManPowerDetails();
	}

	@GetMapping(value = "/getJobProfileChanges")
	public void getJobProfileChanges() {
		logger.info("Requesting for getJobProfileChanges");
		manpowerIntegrationSchedulerService.getJobProfileChanges();
	}
	
	@GetMapping(value = "/sendManpowerLogMail")
	public void sendManpowerLogMail(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for manpowerLogMail");
		manpowerIntegrationSchedulerService.getManpowerLogMail();
	}

	@GetMapping(value = "/encryptAllMigratedCitizenshipNationality")
	public void encryptAllCitizenshipNationality(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for encryptAllMigratedCitizenshipNationality");
		manpowerIntegrationSchedulerService.encryptAllMigratedCitizenshipNationality();
	}

	@PostMapping(value = "/retriggerAwardWorkdayPrerequisite")
	public String retriggerAwardPrerequisuites(@RequestBody ManpowerIntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for retriggerAwardWorkdayPrerequisite");
		logger.info("Award Number : {}", vo.getAwardNumber());
		logger.info("Workday Manpower Interface Id : {}", vo.getWorkdayManpowerInterfaceId());
		return manpowerIntegrationSchedulerService.retriggerAwardWorkdayPrerequisite(vo);
	}

	@PostMapping(value = "/retriggerWorkdayApi")
	public String retriggerWorkdayApi(@RequestBody ManpowerIntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for retriggerWorkdayApi");
		logger.info("workdaymanpowerInterfaceId : {}", vo.getWorkdayManpowerInterfaceId());
		logger.info("manpowerLogId : {}", vo.getManpowerLogId());
		return manpowerIntegrationSchedulerService.retriggerWorkdayApi(vo);
	}

	@PostMapping(value = "/getManpowerLookupSyncDetails")
	public String getManpowerLookupSyncDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getManpowerLookupSyncDetails");		
		return manpowerIntegrationService.getManpowerLookupSyncDetails();
	}

	@PostMapping(value = "/getAwardTriggerDetails")
	public String getAwardTriggerDetails(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAwardTriggerDetails");		
		return manpowerIntegrationService.getAwardTriggerDetails(vo);
	}

	@PostMapping(value = "/getPositionTriggerDetails")
	public String getPositionTriggerDetails(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getPositionTriggerDetails");		
		return manpowerIntegrationService.getPositionTriggerDetails(vo);
	}

	@PostMapping(value = "/getPositionErrorDetails")
	public String getPositionErrorDetails(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getPositionErrorDetails");		
		return manpowerIntegrationService.getPositionErrorDetails(vo);
	}

	@PostMapping(value = "/getCostAllocationTriggerDetails")
	public String getCostAllocationTriggerDetails(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCostAllocationTriggerDetails");		
		return manpowerIntegrationService.getCostAllocationTriggerDetails(vo);
	}

	@PostMapping(value = "/getCurrentCostAllocationDetails")
	public String getCurrentCostAllocationDetails(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCurrentCostAllocationDetails");		
		return manpowerIntegrationService.getCurrentCostAllocationDetails(vo);
	}

	@PostMapping(value = "/updateManpowerInterfaceManually")
	public String updateManpowerInterfaceManually(@RequestBody ManpowerIntegrationVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateManpowerInterfaceManually");		
		return manpowerIntegrationService.updateManpowerInterfaceManually(vo);
	}

	@PostMapping(value = "/retriggerWorkdayClosePosition")
	public String retriggerWorkdayClosePosition(@RequestBody ManpowerIntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for retriggerClosePosition");
		logger.info("workdaymanpowerInterfaceId : {}", vo.getWorkdayManpowerInterfaceId());
		logger.info("manpowerLogId : {}", vo.getManpowerLogId());
		return manpowerIntegrationSchedulerService.retriggerWorkdayClosePosition(vo);
	}

}

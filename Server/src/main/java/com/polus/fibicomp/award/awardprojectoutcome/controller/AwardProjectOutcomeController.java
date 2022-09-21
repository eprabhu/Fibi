package com.polus.fibicomp.award.awardprojectoutcome.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.awardprojectoutcome.dto.AwardOutcomeDTO;
import com.polus.fibicomp.award.awardprojectoutcome.service.AwardProjectOutcomeService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class AwardProjectOutcomeController {

	protected static Logger logger = LogManager.getLogger(AwardProjectOutcomeController.class.getName());

	@Autowired
	private AwardProjectOutcomeService awardProjectOutcomeService;

	@PostMapping(value = "/findPublications")
	public String findPublications(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Publications");
		return awardProjectOutcomeService.findPublications(vo);
	}

	@PostMapping(value = "/saveAwardPublication")
	public String saveAwardPublication(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardPublication");
		return awardProjectOutcomeService.saveAwardPublication(vo);
	}

	@PostMapping(value = "/deleteAwardPublication")
	public String deleteAwardPublication(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for delete Publications");
		return awardProjectOutcomeService.deleteAwardPublication(vo);
	}

	@PostMapping(value = "/loadAllAwardProjectOutcomes")
	public String loadAllAwardProjectOutcomes(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for loadAllAwardProjectOutcomes");
		return awardProjectOutcomeService.loadAllAwardProjectOutcomes(vo);
	}

	@PostMapping(value = "/saveAwardAssociation")
	public String saveAwardAssosiation(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardAssociation");
		return awardProjectOutcomeService.saveAwardAssociation(vo);
	}

	@PostMapping(value = "/deleteAwardAssociation")
	public String deleteAwardAssosiation(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteAwardAssociation");
		return awardProjectOutcomeService.deleteAwardAssociation(vo);
	}

	@PostMapping(value = "/addAwardAcheivements")
	public String addAwardAcheivements(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addAwardAcheivements");
		return awardProjectOutcomeService.addAwardAcheivements(files, formDataJson);
	}

	@PostMapping(value = "/deleteAwardAcheivements")
	public String deleteAwardAcheivements(@RequestBody AwardVO vo, HttpServletRequest request) {
		logger.info("Requesting for deleteAwardAcheivements");
		return awardProjectOutcomeService.deleteAwardAcheivements(vo);
	}

	@GetMapping(value = "/downloadAwardAcheivementsAttachment")
	public ResponseEntity<byte[]> downloadAwardAcheivementsAttachment(HttpServletResponse response, @RequestParam("awardAcheivementId") String awardAcheivementAttachId) {
		logger.info("Requesting for downloadAwardAcheivementsAttachment");
		logger.info("Award Acheivement id : {}", awardAcheivementAttachId);
		Integer id = Integer.parseInt(awardAcheivementAttachId);
		return awardProjectOutcomeService.downloadAwardAcheivementsAttachment(id);
	}

	@PostMapping(value = "/addAwardAcheivementsForWaf")
	public String addAwardAcheivementsForWaf(@RequestBody AwardVO vo, HttpServletRequest request) {
		logger.info("Requesting for addAwardAcheivementsForWaf");
		return awardProjectOutcomeService.addAwardAcheivementsForWaf(vo);
	}

	@PostMapping(value = "/getModuleDetails")
	public String getModuleDetails(@RequestBody AwardOutcomeDTO vo, HttpServletRequest request) {
		logger.info("Requesting for getModuleDetails");
		return awardProjectOutcomeService.getModuleDetails(vo);
	}
}

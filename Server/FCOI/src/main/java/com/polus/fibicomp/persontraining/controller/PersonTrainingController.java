package com.polus.fibicomp.persontraining.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.persontraining.service.PersonTrainingService;
import com.polus.fibicomp.person.vo.PersonVO;

@RestController
public class PersonTrainingController {
	
	protected static Logger logger = LogManager.getLogger(PersonTrainingController.class.getName());
	
	@Autowired
	private PersonTrainingService personTrainingService;
	
	@PostMapping(value = "/saveOrUpdatePersonTraining")
	public String saveOrUpdatePersonTraining(@RequestBody PersonVO vo) {
		logger.info("Request for saveOrUpdatePersonTraining");
		return personTrainingService.saveOrUpdatePersonTraining(vo);
	}
	
	@PostMapping(value = "/getTrainingDashboard")
	public String getTrainingDashboard(@RequestBody PersonVO vo) {
		logger.info("Request for getTrainingDashboard");
		return personTrainingService.getTrainingDashboard(vo);
	}
	
	@PostMapping(value = "/saveOrUpdateTrainingComments")
	public String saveOrUpdateTrainingComments(@RequestBody PersonVO vo) {
		logger.info("Request for saveOrUpdateTrainingComments");
		return personTrainingService.saveOrUpdateTrainingComments(vo);
	}
	
	@DeleteMapping(value = "/deleteTrainingComments/{trainingCommentId}")
	public String deleteTrainingComments(@PathVariable(value = "trainingCommentId", required = true) final Integer trainingCommentId) {
		logger.info("Request for deleteTrainingComments");
		return personTrainingService.deleteTrainingComments(trainingCommentId);
	}
	
	@PostMapping(value = "/saveOrUpdateTrainingAttachment", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateTrainingAttachment(@RequestParam(value = "fileData", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Request for adding saveOrUpdateTrainingAttachment");
		return personTrainingService.saveOrUpdateTrainingAttachment(files, formDataJson);
	}
	
	@GetMapping(value = "/getPersonTrainingDetails/{trainingId}")
	public String getPersonTrainingDetails(@PathVariable(value = "trainingId", required = true) final Integer trainingId) {
		logger.info("Request for getPersonTrainingDetails");
		return personTrainingService.getPersonTrainingDetails(trainingId);
	}
	
	@PostMapping(value = "/loadTrainingList")
	public String loadtraininglist(@RequestBody PersonVO vo) {
		logger.info("Request for loadTrainingList");
		return personTrainingService.loadTrainingList(vo.getSearchString());
	}
	
	@GetMapping(value = "/downloadtrainingAttachment/{trainingAttachmentId}")
	public ResponseEntity<byte[]> downloadtrainingAttachment(@PathVariable(value = "trainingAttachmentId", required = true) final Integer trainingAttachmentId) {
		logger.info("Requesting for downloadtrainingAttachment");
		return personTrainingService.downloadtrainingAttachment(trainingAttachmentId);
	}
	
	@DeleteMapping(value = "/deleteTrainingAttachment/{trainingAttachmentId}")
	public String deleteTrainingAttachment(@PathVariable(value = "trainingAttachmentId", required = true) final Integer trainingAttachmentId) {
		logger.info("Request for deleteTrainingAttachment");
		return personTrainingService.deleteTrainingAttachment(trainingAttachmentId);
	}
	
	@DeleteMapping(value = "/deletePersonTraining/{personTrainingId}")
	public String deletePersonTraining(@PathVariable(value = "personTrainingId", required = true) final Integer personTrainingId) {
		logger.info("Request for deletePersonTraining");
		return personTrainingService.deletePersonTraining(personTrainingId);
	}
}

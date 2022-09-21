package com.polus.fibicomp.codetable.controller;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.codetable.dto.CodeTableDatabus;
import com.polus.fibicomp.codetable.service.CodeTableSerivce;
import com.polus.fibicomp.common.dao.CommonDao;

@Controller
public class CodeTableController {

	protected static Logger logger = LogManager.getLogger(CodeTableController.class.getName());

	@Autowired
	private CodeTableSerivce codeTableService;

	@Autowired
	private CommonDao commonDao;

	@GetMapping(value = "/getCodeTableMetaData")
	public ResponseEntity<String> fetchCodeTableDetails(HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for getCodeTableMetaData");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(codeTableService.getCodeTableConfiguration()), HttpStatus.OK);
	}

	@PostMapping(value = "/getCodeTable")
	public ResponseEntity<String> getCodeTable(@RequestBody CodeTableDatabus codeTableDatabus, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for getCodeTable");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(codeTableService.getTableDetail(codeTableDatabus)), HttpStatus.OK);
	}

	@PostMapping(value = "/updateCodeTableRecord")
	public ResponseEntity<String> updateCodeTableRecord(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws IOException {
		logger.info("Requesting for updateCodeTableRecord");
		ObjectMapper mapper = new ObjectMapper();
		return new ResponseEntity<>(commonDao.convertObjectToJSON(codeTableService.updateCodeTableRecord(mapper.readValue(formDataJson, CodeTableDatabus.class), files)), HttpStatus.OK);
	}

	@PostMapping(value = "/deleteCodeTableRecord")
	public ResponseEntity<String> deleteCodeTableRecord(@RequestBody CodeTableDatabus codeTableDatabus, HttpServletRequest request, HttpServletResponse response) throws JsonProcessingException {
		logger.info("Requesting for deleteCodeTableRecord");
		return new ResponseEntity<>(commonDao.convertObjectToJSON(codeTableService.deleteCodeTableRecord(codeTableDatabus)), HttpStatus.OK);
	}

	@PostMapping(value = "/downloadAttachment")
	public ResponseEntity<byte[]> downloadAttachment(HttpServletResponse response, @RequestBody CodeTableDatabus codeTableDatabus) {
		logger.info("Requesting for downloadAttachment");
		return codeTableService.downloadAttachments(codeTableDatabus, response);
	}

	@PostMapping(value = "/addCodeTableRecord")
	public ResponseEntity<String> addCodeTableRecord(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws IOException {
		logger.info("Requesting for addCodeTableRecord");
		ObjectMapper mapper = new ObjectMapper();
		return new ResponseEntity<>(commonDao.convertObjectToJSON(codeTableService.addCodeTableRecord(files, mapper.readValue(formDataJson, CodeTableDatabus.class))), HttpStatus.OK);
	}

	@PostMapping(value = "/updateCodeTableRecordForWaf")
	public ResponseEntity<String> updateCodeTableRecord(@RequestBody CodeTableDatabus codeTableDatabus, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addCodeTableRecordForWaf");
		return new ResponseEntity<>(codeTableService.updateCodeTableRecordForWaf(codeTableDatabus), HttpStatus.OK);
	}

	@PostMapping(value = "/addCodeTableRecordForWaf")
	public ResponseEntity<String> addCodeTableRecord(@RequestBody CodeTableDatabus codeTableDatabus, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addCodeTableRecordForWaf");
		return new ResponseEntity<>(codeTableService.addCodeTableRecordForWaf(codeTableDatabus), HttpStatus.OK);
	}

}

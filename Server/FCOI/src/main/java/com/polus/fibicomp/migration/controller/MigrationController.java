package com.polus.fibicomp.migration.controller;

import java.io.File;
import java.util.HashSet;
import java.util.Hashtable;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.migration.service.MigrationService;

@RestController
public class MigrationController {

	protected static Logger logger = LogManager.getLogger(MigrationController.class.getName());

	@Autowired
	private MigrationService migrationService;

	@Value("${path.attachment.path}")
	private String attachmentPath;

	@Value("${path.grantcall.attachment.path}")
	private String granCallAttachmentPath;

	@PostMapping(value = "/attachmentDataMigration", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void dataMigration(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for attachmentDataMigration");
		logger.info("PathDirectoryName : {}", attachmentPath);
		Hashtable<String, String> hashTable = new Hashtable<String, String>();
		HashSet<String> wbs = new HashSet<>();
		long totalFiles = migrationService.fileCount(new File(attachmentPath).toPath());
		int loopCounter = -1;
		File file = null;
		do {
			try {
				int counter = 0;
				//remainingFileCount = migrationService.fileCount(new File(attachmentPath).toPath());
				file = migrationService.uploadFile(attachmentPath, hashTable, counter, wbs);
				if(file != null) {
					file.delete();
				}
				/*
				 * if(file != null) { file.delete(); }
				 */
				/*if (!wbs.isEmpty()) {
					for (String wbsno : wbs) {
						migrationService.migrationAttachmentFeed(wbsno);
						logger.info("migrationAttachmentFeed WBSNUMBER{}", wbsno);
					}
					wbs.clear();
				}*/
			} catch (Exception e) {
				logger.info("Exception in attachmentDataMigration", e);
			}
			loopCounter++;
			logger.info("hashcheck : {}", loopCounter + "<" + totalFiles);
		} while (loopCounter<=  totalFiles);
		logger.info("hashTable size : {}", hashTable.size());
		logger.info("Migration process compleated");
	}

	@PostMapping(value = "/grantCallAttachmentDataMigration", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void grantCallAttachmentDataMigration(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for grantCallAttachmentDataMigration");
		logger.info("PathDirectoryName : {}", attachmentPath);
		Hashtable<String, String> hashTable = new Hashtable<String, String>();
		HashSet<String> grantcallIds = new HashSet<>();
		long totalFiles = migrationService.fileCount(new File(granCallAttachmentPath).toPath());
		int loopCounter = -1;
		File file = null;
		do {
			try {
				int counter = 0;
				file = migrationService.grantCallUploadFile(granCallAttachmentPath, hashTable, counter, grantcallIds);
				if (file != null) {
					file.delete();
				}
			} catch (Exception e) {
				logger.info("Exception in grantCallAttachmentDataMigration", e);
			}
			loopCounter++;
			logger.info("hashcheck : {}", loopCounter + "<" + totalFiles);
		} while (loopCounter <= totalFiles);
		logger.info("hashTable size : {}", hashTable.size());
//		Integer avType = 1;
//		migrationService.grantCallAttachmentDataMigrationFeed(avType);
		logger.info("GrantCallAttachmentDataMigration process compleated");
	}

}

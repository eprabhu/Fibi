package com.polus.fibicomp.migration.service;

import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.migration.pojo.TempAttachmentMigration;

@Transactional
@Service(value = "migrationService")
public interface MigrationService {

	/**
	 * This method is used for save the attachments in a temp table
	 * 
	 * @param tempAttachmentMigration
	 * @return
	 */
	public void saveOrUpdateMigrationData(TempAttachmentMigration tempAttachmentMigration);

	/**
	 * this method contains the setup for upload files
	 * 
	 * @param directoryName
	 * @param files
	 */
	public File uploadFile(String directoryName, Hashtable<String, String> hashTable, int counter, HashSet<String> wbs);

	/**
	 * this method is used for log errors
	 * @param wbsNumber - wbsNumber
	 */
	public void migrationAttachmentFeed(String wbsNumber);
	
	public long fileCount(Path dir);

	public File grantCallUploadFile(String attachmentPath, Hashtable<String, String> hashTable, int counter,
			HashSet<String> grantcallIds);

	public void grantCallAttachmentDataMigrationFeed(Integer avType);
	
}

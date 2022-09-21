package com.polus.fibicomp.migration.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.migration.pojo.MigrationAttachmentErrorLog;
import com.polus.fibicomp.migration.pojo.TempAttachmentMigration;

@Transactional
@Service
public interface MigrationDao {

	/**
	 * this method is used for save or update the data
	 * 
	 * @param tempAttachmentMigration
	 * @return
	 */
	public void saveOrUpdateMigrationData(TempAttachmentMigration tempAttachmentMigration);

	/**
	 * This method is used for check the files are already exists
	 * 
	 * @param fileName
	 * @param projectId
	 * @param attachmentType
	 * @return
	 */
	public boolean checkFileAlreadyExist(String fileName, String projectId, String attachmentType);
	
	/**
	 * this method is used for log errors
	 * @param wbsNumber - wbsNumber
	 */
	public void migrationAttachmentFeed(String wbsNumber);

	/**
	 * 
	 * @param migrationAttachmentErrorLog
	 */
	public void saveErrorLog(MigrationAttachmentErrorLog migrationAttachmentErrorLog);

	/**
	 * 
	 * @return
	 */
	public Integer getMaxId();

	public void grantCallAttachmentDataMigrationFeed(Integer avType);

}

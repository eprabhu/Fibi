package com.polus.fibicomp.integration.dao;

import java.util.List;

import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.integration.pojo.AwardFeed;
import com.polus.fibicomp.integration.pojo.AwardHoursLogRT;
import com.polus.fibicomp.integration.pojo.ExpenseTrackerRT;
import com.polus.fibicomp.integration.pojo.FeedAwardDetail;
import com.polus.fibicomp.integration.pojo.TempFeedSDC;
import com.polus.fibicomp.task.pojo.TaskAttachment;

public interface IntegrationDao {

	/**
	 * This method is used to add Award Hours Log RT.
	 * @param awardHoursLogRT - Object of AwardHoursLogRT class.
	 */
	public void addAwardHoursLogRT(AwardHoursLogRT awardHoursLogRT);

	/**
	 * This method is used to add Award Hours Log RT.
	 * @param expenseTrackerRT - Object of ExpenseTrackerRT class.
	 */
	public void addExpenseTrackerRT(ExpenseTrackerRT expenseTrackerRT);

	/**
	 * This method is used to delete all data Award Hours Log RT.
	 */
	public void deleteAllAwardHoursLogRT();

	/**
	 * This method is used to delete all data Expense Tracker RT.
	 */
	public void deleteAllExpenseTrackerRT();

	/**
	 * This method is used to add Publication.
	 * @param Publication - Object of Publication class.
	 */
	public void addPublication(Publication publication);

	/**
	 * This method is used to delete all Publications.
	 */
	public void deleteAllPublications();

	/**
	 * This method is used to save SAP Award details of an active award.
	 * @param awardId - awardId
	 * @param awardNumber - awardNumber
	 * @param sequenceNumber - sequenceNumber
	 * @param updateUser - updateUser
	 */
	public void saveSAPAwardDetails(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser);

	/**
	 * This methd is used to get the users to feed the new award or proposal
	 * @return
	 */
	public List<AwardFeed> getTempUsers();

	/**
	 * This method is used to save the feeded award details
	 * @param feedAwardDetail
	 */
	public void saveFeededAwardDetails(FeedAwardDetail feedAwardDetail);

	/**
	 * This method is used to fetch instructions by task type code
	 * @param taskTypeCode
	 * @return instruction
	 */
	public String fetchInstuctionsByTaskTypeCode(String taskTypeCode);

	/**
	 * This method is used to get the user details for sdc
	 * @return list of users
	 */
	public List<TempFeedSDC> getSDCFeeds();

	/**
	 * This method is used to get all task attachments
	 * @param taskId
	 * @return all attachments
	 */
	public List<TaskAttachment> getAllTaskAttachments(Integer taskId);

	/**
	 * This method is used to get the account number based on award Id
	 * @param awardId
	 * @return account number
	 */
	public String getAccontNumberByAwardId(Integer awardId);

	/**
	 * This method is used to check for the sap feed
	 * @param awardNumber
	 * @param awardId 
	 * @return true- if can add sap feed else false
	 */
	public String checkForTheSAPFeedResponse(String awardNumber, Integer awardId);

}

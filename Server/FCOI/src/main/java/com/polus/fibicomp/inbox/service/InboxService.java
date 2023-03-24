package com.polus.fibicomp.inbox.service;

import com.polus.fibicomp.inbox.vo.InboxVO;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.task.pojo.Task;

public interface InboxService {
		
	/**
	 * 
	 * @param inboxVO
	 * @return
	 */
	String markReadMessage(InboxVO inboxVO);
	
	/**
	 * 
	 * @param inboxVO	
	 * @return
	 */
	String showInbox(InboxVO inboxVO);


	/**
	 * This method will add data to Inbox
	 * @param proposal- object of proposal
	 * @param reviewerPersonId - id of reviewer
	 * @param updateUser - updateUser
	 * @param messageTypeCode - messageTypeCode
	 * @param subjectTypeCode - subjectTypeCode
	 * @param reviewId - reviewId
	 */
	public void addMessageToInbox(Proposal proposal, String reviewerPersonId, String updateUser, String messageTypeCode,
			String subjectTypeCode, Integer reviewId, String description);

	/**
	 * This method will remove Message From Inbox
	 * @param moduleItemKey- moduleItemKey
	 * @param reviewId - id of reviewer
	 * @param moduleCode - moduleCode
	 */
	public void removeMessageFromInbox(Integer moduleItemKey, Integer reviewId, Integer moduleCode);

	/**
	 * This method will add award message to inbox
	 * @param awardId - awardId
	 * @param reviewerPersonId - id of reviewer
	 * @param updateUser - updateUser
	 * @param messageTypeCode - messageTypeCode
	 * @param subjectTypeCode - subjectTypeCode
	 * @param subModuleItemKey - subModuleItemKey
	 * @param description - description
	 * @param subModuleCode - subModuleCode
	 * @param userMessage - userMessage
	 */
	public void addAwardMessageToInbox(String awardId, String reviewerPersonId, String updateUser, String messageTypeCode, String subjectTypeCode, Integer subModuleItemKey, String description, Integer subModuleCode, String userMessage);

	/**
	 * This method will add award message to inbox
	 * @param task - object of Task
	 * @param updateUser - updateUser
	 * @param messageTypeCode - messageTypeCode
	 * @param subjectTypeCode - subjectTypeCode
	 * @param subModuleCode - subModuleCode
	 * @param userMessage - userMessage
	 */
	public void addTaskMessageToInbox(Task task, String updateUser, String messageTypeCode, String subjectTypeCode, Integer subModuleCode, String userMessage);

	/**
	 * This method will remove Message From Inbox
	 * @param moduleItemKey- moduleItemKey
	 * @param subModuleItemKey - subModuleItemKey
	 * @param moduleCode - moduleCode
	 * @param subModuleCode - subModuleCode
	 */
	public void removeTaskMessageFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode, Integer subModuleCode);

	/**
	 * This method is used to add message 
	 * @param delegationId
	 * @param personId
	 * @param updateUser
	 * @param messageTypeCodes
	 * @param subjectTypeCode
	 * @param userMessage
	 */
	public void addDelegationMessageToInbox(String delegationId, String personId, String updateUser, String messageTypeCode, String subjectTypeCode, String userMessage);

	/**
	 * This method will add agreement message to inbox
	 * @param task - object of Task
	 * @param updateUser - updateUser
	 * @param messageTypeCode - messageTypeCode
	 * @param subjectTypeCode - subjectTypeCode
	 * @param subModuleCode - subModuleCode
	 * @param userMessage - userMessage
	 */
	public void addAgreementMessageToInbox(String moduleItemKey, String assigneePersonId, String updateUser,
			String messageTypeCode, String subjectTypeCode, Integer subModuleItemKey, Integer subModuleCode,
			String userMessage);

	/**
	 * @param moduleItemKey
	 * @param assigneePersonId
	 * @param messageTypeCode
	 * @param subjectTypeCode
	 * @param subModuleItemKey
	 * @param subModuleCode
	 * @param userMessage
	 * @param moduleCode
	 */
	void addMessageToInbox(String moduleItemKey, String assigneePersonId, String messageTypeCode,
			Integer subModuleItemKey, Integer subModuleCode, String userMessage,
			Integer moduleCode);

}

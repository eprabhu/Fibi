package com.polus.fibicomp.inbox.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.inbox.vo.InboxVO;

@Service
public interface InboxDao {

	/**
	 * @param invoxVO
	 * @return
	 */
	public void markReadMessage(InboxVO inboxVO);

	/**
	 * @param invoxVO
	 * @param isViewAll
	 * @return
	 */
	public List<Inbox> showInbox(InboxVO inboxVO);

	/**
	 * This method will save to Inbox
	 * @param inbox object
	 * @return
	 */
	public void saveToInbox(Inbox inbox);

	/**
	 * This method is used to mark message as read
	 * @param moduleCode - moduleCode
	 * @param moduleItemKey - moduleItemKey
	 * @param personId - personId
	 * @param messageTypeCode - messageTypeCode
	 * @param subModuleCode - subModuleCode
	 * @param subModuleItemKey -subModuleItemKey
	 */
	public void markReadMessage(Integer moduleCode, String moduleItemKey, String personId, String messageTypeCode, String subModuleItemKey, Integer subModuleCode);

	/**
	 * This method is used to remove message from inbox
	 * @param moduleItemKey - moduleItemKey
	 * @param subModuleItemKey - subModuleItemKey
	 * @param moduleCode - moduleCode
	 */
	public void removeFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode);

	/**
	 * This method is used to mark message as read
	 * @param moduleCode - moduleCode
	 * @param moduleItemKey - moduleItemKey
	 * @param messageTypeCode - messageTypeCode
	 * @param subModuleCode - subModuleCode
	 */
	public void markAsReadBasedOnParams(Integer moduleCode, String moduleItemKey, String messageTypeCode);

	/**
	 * This method is used to remove message from inbox
	 * @param moduleItemKey - moduleItemKey
	 * @param subModuleItemKey - subModuleItemKey
	 * @param moduleCode - moduleCode
	 * @param subModuleCode - subModuleCode
	 */
	public void removeTaskMessageFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode, Integer subModuleCode);

	/**
	 * This method is used to delete deleteOldAssigneeInbox from actionList.
	 * @param taskId - taskId. 
	 * @param moduleCode - moduleCode.
	 * @param moduleItemId - moduleItemId.
	 * @param awardTaskSubModuleCode - awardTaskSubModuleCode.
	 * @param messageTypeAssignTask - messageTypeAssignTask.
	 * @param oldAssigneePersonId - oldAssigneePersonId.
	 * @param subModuleItemKey - subModuleItemKey.
	 */
	public void deleteMessageFromInboxByParams(String subModuleItemKey, Integer moduleCode, String moduleItemId, Integer awardTaskSubModuleCode, String messageTypeAssignTask, String oldAssigneePersonId);

	/**
	 * This method is used to mark the actions as expired
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param submoduleCode
	 * @param submoduleItemKey
	 */
	public void markAsExpiredFromActionList(Integer moduleCode, String moduleItemKey, Integer submoduleCode, String submoduleItemKey);

	/**
	 * This method is used to update message from inbox
	 * @param moduleCode
	 * @param messageTypeCodes
	 * @param submoduleItemKey
	 */
	public void updateMessageInboxByParam(String subModuleItemKey, Integer moduleCode, String messageTypeCode);
	
	public Inbox saveBannerEntriesToActionList(Inbox actionList);

}

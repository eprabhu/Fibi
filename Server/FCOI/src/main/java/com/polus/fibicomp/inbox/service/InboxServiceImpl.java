package com.polus.fibicomp.inbox.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.inbox.vo.InboxVO;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.task.pojo.Task;

@Transactional
@Service(value = "inboxService")
public class InboxServiceImpl implements InboxService {

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private CommonDao commonDao;

	@Override
	public String markReadMessage(InboxVO inboxVO) {
		inboxDao.markReadMessage(inboxVO);
		return commonDao.convertObjectToJSON("Success");
	}

	@Override
	public String showInbox(InboxVO inboxVO) {
		inboxVO.setInboxDetails(inboxDao.showInbox(inboxVO));
		inboxVO.setModules(commonDao.getModules());
		return commonDao.convertObjectToJSON(inboxVO);
	}

	@Override
	public void addMessageToInbox(Proposal proposal, String reviewerPersonId, String updateUser, String messageTypeCode,
			String subjectTypeCode, Integer reviewId, String description) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
		inbox.setSubModuleCode(Constants.PROPOSAL_EVALUATION_SUBMODULE_CODE);
		inbox.setModuleItemKey(proposal.getProposalId().toString());
		inbox.setSubModuleItemKey(reviewId.toString());
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(reviewerPersonId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(updateUser);
		if(commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			String applicationId = "";
			if(proposal.getApplicationId() != null) {
				applicationId = proposal.getApplicationId();
			}
			inbox.setUserMessage(applicationId + " - " + proposal.getTitle()+ " - "+ description +" Review");
		} else {
			inbox.setUserMessage(proposal.getProposalId().toString() + " - " + proposal.getTitle() + " - " + description +" Review");
		}
		inboxDao.saveToInbox(inbox);
	}

	public void removeMessageFromInbox(Integer moduleItemKey, Integer reviewId, Integer moduleCode) {
		inboxDao.removeFromInbox(moduleItemKey, reviewId, moduleCode);
	}

	@Override
	public void addAwardMessageToInbox(String awardId, String reviewerPersonId, String updateUser, String messageTypeCode, String subjectTypeCode, Integer subModuleItemKey, String description, Integer subModuleCode, String userMessage) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(Constants.AWARD_MODULE_CODE);
		inbox.setSubModuleCode(subModuleCode);
		inbox.setModuleItemKey(awardId);
		inbox.setSubModuleItemKey(subModuleItemKey.toString());
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(reviewerPersonId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(updateUser);
		inbox.setUserMessage(userMessage);
		inboxDao.saveToInbox(inbox);
	}

	@Override
	public void addTaskMessageToInbox(Task task, String updateUser, String messageTypeCode, String subjectTypeCode, Integer subModuleCode, String userMessage) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(task.getModuleCode());
		inbox.setSubModuleCode(subModuleCode);
		inbox.setModuleItemKey(task.getModuleItemId());
		inbox.setSubModuleItemKey(task.getTaskId().toString());
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(task.getAssigneePersonId());
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(updateUser);
		inbox.setUserMessage(userMessage);
		inboxDao.saveToInbox(inbox);
	}

	public void removeTaskMessageFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		inboxDao.removeTaskMessageFromInbox(moduleItemKey, subModuleItemKey, moduleCode, subModuleCode);
	}

	@Override
	public void addDelegationMessageToInbox(String delegationId, String personId, String updateUser, String messageTypeCode, String subjectTypeCode, String userMessage) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleItemKey(personId);
		inbox.setModuleCode(Constants.PERSON_MODULE_CODE);
		inbox.setSubModuleCode(Constants.PERSON_SUBMODULE_CODE);
		inbox.setSubModuleItemKey(delegationId);
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(personId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(updateUser);
		inbox.setUserMessage(userMessage);
		inboxDao.saveToInbox(inbox);
	}

	@Override
	public void addAgreementMessageToInbox(String moduleItemKey, String assigneePersonId, String updateUser,
			String messageTypeCode, String subjectTypeCode, Integer subModuleItemKey, Integer subModuleCode,
			String userMessage) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		inbox.setSubModuleCode(subModuleCode);
		inbox.setModuleItemKey(moduleItemKey);
		inbox.setSubModuleItemKey(subModuleItemKey.toString());
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType(subjectTypeCode);
		inbox.setToPersonId(assigneePersonId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(updateUser);
		inbox.setUserMessage(userMessage);
		inboxDao.saveToInbox(inbox);
	}

	@Override
	public void addMessageToInbox(String moduleItemKey, String assigneePersonId,
			String messageTypeCode, Integer subModuleItemKey, Integer subModuleCode,
			String userMessage, Integer moduleCode) {
		Inbox inbox = new Inbox();
		inbox.setArrivalDate(commonDao.getCurrentTimestamp());
		inbox.setMessageTypeCode(messageTypeCode);
		inbox.setModuleCode(moduleCode);
		inbox.setSubModuleCode(subModuleCode);
		inbox.setModuleItemKey(moduleItemKey);
		inbox.setSubModuleItemKey(subModuleItemKey.toString());
		inbox.setOpenedFlag(Constants.NO);
		inbox.setSubjectType("P");
		inbox.setToPersonId(assigneePersonId);
		inbox.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		inbox.setUpdateUser(AuthenticatedUser.getLoginUserName());
		inbox.setUserMessage(userMessage);
		inboxDao.saveToInbox(inbox);
	}
}

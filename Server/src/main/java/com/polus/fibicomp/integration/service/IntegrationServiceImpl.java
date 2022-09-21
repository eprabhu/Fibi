package com.polus.fibicomp.integration.service;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.awardworkflow.dao.AwardWorkflowDao;
import com.polus.fibicomp.award.awardworkflow.service.AwardWorkflowService;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.version.service.AwardVersionService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetRateAndBase;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.dao.CustomDataElementDao;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.integration.dao.IntegrationDao;
import com.polus.fibicomp.integration.pojo.AwardFeed;
import com.polus.fibicomp.integration.pojo.AwardHoursLogRT;
import com.polus.fibicomp.integration.pojo.ExpenseTrackerRT;
import com.polus.fibicomp.integration.pojo.FeedAwardDetail;
import com.polus.fibicomp.integration.pojo.TempFeedSDC;
import com.polus.fibicomp.integration.vo.IntegrationVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalExtension;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonAttachment;
import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.proposal.pojo.ProposalPersonUnit;
import com.polus.fibicomp.proposal.service.ProposalCopyService;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskAttachment;
import com.polus.fibicomp.task.pojo.TaskFileData;
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.task.vo.TaskVO;
import com.polus.fibicomp.wbs.dao.WBSDao;

@Transactional
@Service(value = "integrationService")
public class IntegrationServiceImpl implements IntegrationService{

	@Autowired
	private IntegrationDao integrationDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private AwardWorkflowDao awardWorkflowDao;

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private WBSDao wbsDao;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private TaskService taskService;
	
	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private ProposalService proposalService;

	@Autowired
	private ProposalLookUpDao  proposalLookUpDao;

	@Autowired
	private ProposalCopyService proposalCopyService;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private AwardVersionService awardVersionService;

	@Autowired
	private AwardWorkflowService awardWorkflowService;

	@Autowired
	private AuthorizationService authorizationService;

	protected static Logger logger = LogManager.getLogger(IntegrationServiceImpl.class.getName());

	@Override
	public String addAwardHoursLogRT(IntegrationVO vo) {
		List<AwardHoursLogRT> awardHoursLogRTs = vo.getAwardHoursLogRTs();
//		integrationDao.deleteAllAwardHoursLogRT();
		for (AwardHoursLogRT awardHoursLogRT : awardHoursLogRTs) {
			try { 
				awardHoursLogRT.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardHoursLogRT.setUpdateUser("quickstart");
				integrationDao.addAwardHoursLogRT(awardHoursLogRT);
				vo.getMessages().add(awardHoursLogRT.getFundCode() + "Success");
			} catch (Exception e) {
				e.printStackTrace();
				vo.getMessages().add(awardHoursLogRT.getFundCode() + "Error Occured");
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addExpenseTrackerRT(IntegrationVO vo) {
		List<ExpenseTrackerRT> expenseTrackerRTs = vo.getExpenseTrackerRTs();
//		integrationDao.deleteAllExpenseTrackerRT();
		for (ExpenseTrackerRT expenseTrackerRT : expenseTrackerRTs) {
			try {
				expenseTrackerRT.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				expenseTrackerRT.setUpdateUser("quickstart");
				integrationDao.addExpenseTrackerRT(expenseTrackerRT);
				vo.getMessages().add(expenseTrackerRT.getFiGlAccount() + "Success");
			} catch (Exception e) {
				e.printStackTrace();
				vo.getMessages().add(expenseTrackerRT.getFiGlAccount() + "Error Occured");
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addPublication(IntegrationVO vo) {
		List<Publication> publications = vo.getPublications();
		//integrationDao.deleteAllPublications();
		for (Publication publication : publications) {
			try {
				publication.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				publication.setUpdateUser("quickstart");
				integrationDao.addPublication(publication);
				vo.getMessages().add(publication.getNameOfJournal() + "Success");
			} catch (Exception e) {
				e.printStackTrace();
				vo.getMessages().add(publication.getNameOfJournal() + "Error Occured");
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardHoursLogRT() {
		String message = "";
		try {
			integrationDao.deleteAllAwardHoursLogRT();
			message = "Success";
		} catch (Exception e) {
			e.printStackTrace();
			message = "Error Occured";
		}
		return message;
	}

	@Override
	public String deleteExpenseTrackerRT() {
		String message = "";
		try {
			integrationDao.deleteAllExpenseTrackerRT();
			message = "Success";
		} catch (Exception e) {
			e.printStackTrace();
			message = "Error Occured";
		}
		return message;
	}

	@Override
	public void saveSAPAwardDetails(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser) {
		integrationDao.saveSAPAwardDetails(awardId, awardNumber, sequenceNumber, updateUser);
	}

	@Override
	public String seedAward(IntegrationVO vo) {
		try {
			if (vo.getPiFeed().equals(Boolean.TRUE)) {
				List<AwardFeed> awardFeeds = integrationDao.getTempUsers();
				for (AwardFeed awardFeed : awardFeeds) {
					vo.setPersonName(awardFeed.getUserName());
					vo.setUnitNumber(awardFeed.getUnitNumber());
					vo.setUpdateUser(awardFeed.getCreatedUserName());
					if (personDao.getPersonIdByUserName(awardFeed.getCreatedUserName()) != null
							&& personDao.getPersonIdByUserName(awardFeed.getUserName()) != null) {
						copyAward(vo);
					}
				}
			} else {
				List<TempFeedSDC> tempFeedSDCs = integrationDao.getSDCFeeds();
				for (TempFeedSDC tempFeedSDC : tempFeedSDCs) {
					Person piPerson = personDao.getPersonDetailById(tempFeedSDC.getPiPersonid());
					Person createPerson = personDao.getPersonDetailById(tempFeedSDC.getPersonId());
					if (piPerson.getPrincipalName() != null && createPerson.getPrincipalName() != null) {
						vo.setPersonName(piPerson.getPrincipalName());
						vo.setUnitNumber(tempFeedSDC.getDeptCode());
						vo.setUpdateUser(createPerson.getPrincipalName());
						copyAward(vo);
					}
				}
			}
			return "success";
		} catch (Exception e) {
			e.printStackTrace();
			return "Failed";
		}
	}

	@Override
	public String feedProposal(IntegrationVO vo) {
		try {
			if (vo.getPiFeed().equals(Boolean.TRUE)) {
				List<AwardFeed> awardFeeds = integrationDao.getTempUsers();
				for (AwardFeed awardFeed : awardFeeds) {
					vo.setPersonName(awardFeed.getUserName());
					vo.setUnitNumber(awardFeed.getUnitNumber());
					vo.setUpdateUser(awardFeed.getCreatedUserName());
					if (personDao.getPersonIdByUserName(awardFeed.getCreatedUserName()) != null
							&& personDao.getPersonIdByUserName(awardFeed.getUserName()) != null) {
						copyProposal(vo);
					}
				}
			} else {
				List<TempFeedSDC> tempFeedSDCs = integrationDao.getSDCFeeds();
				for (TempFeedSDC tempFeedSDC : tempFeedSDCs) {
					Person piPerson = personDao.getPersonDetailById(tempFeedSDC.getPiPersonid());
					Person createPerson = personDao.getPersonDetailById(tempFeedSDC.getPersonId());
					if (piPerson.getPrincipalName() != null && createPerson.getPrincipalName() != null) {
						vo.setPersonName(piPerson.getPrincipalName());
						vo.setUnitNumber(tempFeedSDC.getDeptCode());
						vo.setUpdateUser(createPerson.getPrincipalName());
						copyProposal(vo);
					}
				}
			}
			return "success";
		} catch (Exception e) {
			e.printStackTrace();
			return "Failed";
		}
	}

	public String copyAward(IntegrationVO vo) {
		Award orginalAward = awardDao.getAwardDetailsById(vo.getAwardId());
		Award copyAward = new Award();
		copyAwardMandatoryFields(vo, copyAward, orginalAward);
		copyAwardNonMandatoryFields(vo, copyAward, orginalAward);
		copyAward = copyAwardBudgetDetails(orginalAward, copyAward, vo);
		if(vo.getActive().equals(Boolean.TRUE)) {
			wbsDao.generateWBSNumber(copyAward.getAwardId(), "Y", null, null);
		}
		copyTask(vo, copyAward);
		if (copyAward.getCreateUser() != null) {
			copyAward.setCreateUserFullName(personDao.getUserFullNameByUserName(copyAward.getCreateUser()));
		}
		if (copyAward.getUpdateUser() != null) {
			copyAward.setUpdateUserFullName(personDao.getUserFullNameByUserName(copyAward.getUpdateUser()));
		}
//		copyAward.setCanCreateVariationRequest(awardService.canCreateVariationRequest(copyAward.getAwardId(), copyAward.getAwardNumber()));
		copyAward.setWorkFlowStatusName(copyAward.getAwardWorkflowStatus().getDescription());
		awardService.moveDataToAwardHierarchy(copyAward);
		if (vo.getCreateVariationRequest().equals(Boolean.TRUE)) {
			copyAward.setAccountNumber(integrationDao.getAccontNumberByAwardId(copyAward.getAwardId()));
			createAwardVariationRequest(vo, copyAward);
		}
		saveFeededAwardDetails(copyAward, vo.getActive(), vo.getScenario());
		return commonDao.convertObjectToJSON(copyAward);
	}

	private void copyTask(IntegrationVO vo, Award copyAward) {
		for (String taskTypeCode : vo.getTaskTypeCodes()) {
			Task task = new Task();
			Timestamp currentTimestamp = commonDao.getCurrentTimestamp();
			task.setTaskTypeCode(taskTypeCode);
			task.setAssigneePersonId(copyAward.getPiPersonId());
			task.setModuleCode(1);
			task.setModuleItemId(copyAward.getAwardId().toString());
			task.setCreateTimestamp(currentTimestamp);
			task.setCreateUser(copyAward.getCreateUser());
			task.setUpdateUser(copyAward.getCreateUser());
			task.setUpdateTimeStamp(currentTimestamp);
			task.setStartModuleSubItemKey(1);
			task.setTaskStatusCode(Constants.TASK_STATUS_CODE_OPEN);
			task.setModuleItemKey(copyAward.getAwardNumber());
			task.setDescription(integrationDao.fetchInstuctionsByTaskTypeCode(taskTypeCode));
			task.setStartDate(currentTimestamp);
			Calendar cal = Calendar.getInstance();
			cal.setTime(currentTimestamp);
			cal.add(Calendar.DAY_OF_WEEK, 30);
			task.setDueDate(new Timestamp(cal.getTime().getTime()));
			if (taskTypeCode.equals("3") || taskTypeCode.equals("8")) {
				task.getTaskAttachments().addAll(copyTaskAttachments(vo.getLoaTaskId(),task, vo.getUpdateUser()));
			} else if (taskTypeCode.equals("4")) {
				task.getTaskAttachments().addAll(copyTaskAttachments(vo.getEthicstaskId(),task, vo.getUpdateUser()));
			}
			taskDao.saveOrUpdateTask(task);
			if(vo.getTaskTypeActive().equals(Boolean.TRUE)) {
				completeTask(task, vo, copyAward.getLeadUnitNumber());				
			}
		}
	}

	private void completeTask(Task task, IntegrationVO vo, String leadUnitNumber) {
		TaskVO taskVO = new TaskVO();
		taskVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		taskVO.setSubModuleCode(Constants.AWARD_TASK_SUBMODULE_CODE);
		taskVO.setModuleItemId(Integer.parseInt(task.getModuleItemId()));
		taskVO.setTaskId(task.getTaskId());
		taskVO.setPersonId(task.getAssigneePersonId());
		taskVO.setUserName(task.getUpdateUser());
		taskVO.setLeadUnitNumber(leadUnitNumber);
		taskVO.setTaskTypeCode(task.getTaskTypeCode());
		taskVO.setUpdateUser(task.getUpdateUser());
		taskVO.setAssigneePersonId(task.getAssigneePersonId());
		taskService.startTask(taskVO);
		taskVO.setEndModuleSubItemKey(1);
		taskService.completeTaskDetails(taskVO);
	}

	private void saveFeededAwardDetails(Award copyAward, Boolean isActive, String scenario) {
		FeedAwardDetail feedAwardDetail = new FeedAwardDetail();
		feedAwardDetail.setAwardId(copyAward.getAwardId());
		feedAwardDetail.setAwardNumber(copyAward.getAwardNumber());
		feedAwardDetail.setUnitName(commonDao.getUnitName(copyAward.getLeadUnitNumber()));
		feedAwardDetail.setUnitNumber(copyAward.getLeadUnitNumber());
		feedAwardDetail.setTitle(copyAward.getTitle());
		feedAwardDetail.setIsActive(isActive.equals(Boolean.TRUE)? "Y" : "N");
		feedAwardDetail.setScenario(scenario);
		List<AwardPerson> awardPersons = copyAward.getAwardPersons();
		for (AwardPerson awardPerson : awardPersons) {
			if(awardPerson.getIsPi().equals(Boolean.TRUE)) {
				feedAwardDetail.setPiFullName(awardPerson.getFullName());
				feedAwardDetail.setPiUserName(awardPerson.getPersonId());
			}
		}
		feedAwardDetail.setCreateUser(copyAward.getCreateUser());
		integrationDao.saveFeededAwardDetails(feedAwardDetail);
	}

	private void copyAwardMandatoryFields(IntegrationVO vo, Award copyAward, Award orginalAward) {
		copyAward.setActivityTypeCode(orginalAward.getActivityTypeCode());
		copyAward.setBeginDate(orginalAward.getBeginDate());
		copyAward.setAccountTypeCode(orginalAward.getAccountTypeCode());
		copyAward.setAccountType(orginalAward.getAccountType());
		copyAward.setFinalExpirationDate(orginalAward.getFinalExpirationDate());
		copyAward.setLeadUnitNumber(vo.getUnitNumber());
		copyAward.setLeadUnit(commonDao.getLeadUnitByUnitNumber(vo.getUnitNumber()));
		copyAward.setSponsorCode(orginalAward.getSponsorCode());
		copyAward.setSponsor(commonDao.getSponsorById(orginalAward.getSponsorCode()));
		if (vo.getPiFeed().equals(Boolean.TRUE) ) {
			copyAward.setTitle(personDao.getUserFullNameByUserName(vo.getPersonName()) + "-" + commonDao.getUnitName(vo.getUnitNumber()) + "-" + vo.getTitle());
		}else {
			copyAward.setTitle(personDao.getUserFullNameByUserName(vo.getUpdateUser()) + "-" + commonDao.getUnitName(vo.getUnitNumber()) + "-" + vo.getTitle());
		}
		copyAward.setResearchDescription(orginalAward.getResearchDescription());
		copyAward.setMultiDisciplinaryDescription(orginalAward.getMultiDisciplinaryDescription());
		copyAward.setCfdaNumber(orginalAward.getCfdaNumber());	
		copyAward.setDfafsNumber(orginalAward.getDfafsNumber());	
		copyAward.setSponsorAwardNumber(orginalAward.getSponsorAwardNumber());	
		if (orginalAward.getPrimeSponsorCode() != null) {	
			copyAward.setPrimeSponsorCode(orginalAward.getPrimeSponsorCode());	
			copyAward.setPrimeSponsor(commonDao.getSponsorById(orginalAward.getPrimeSponsorCode()));	
		}	
		copyAward.setInvoiceInstructions(orginalAward.getInvoiceInstructions());	
		copyAward.setFinalInvoiceDue(orginalAward.getFinalInvoiceDue());	
		copyAward.setInvoiceNoOfCopies(orginalAward.getInvoiceNoOfCopies());	
		copyAward.setPaymentInvoiceFrequencyCode(orginalAward.getPaymentInvoiceFrequencyCode());	
		copyAward.setMethodOfPaymentCode(orginalAward.getMethodOfPaymentCode());	
		copyAward.setBasisOfPaymentCode(orginalAward.getBasisOfPaymentCode());
		copyAward.setUpdateUser(vo.getUpdateUser());
		copyAward.setCreateUser(vo.getUpdateUser());
		copyAward.setCreateTimestamp(commonDao.getCurrentTimestamp());
		copyAward.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyAward.setIsLatest(true);
		copyAward.setAwardTypeCode(orginalAward.getAwardTypeCode());
		copyAward.setActivityTypeCode(orginalAward.getActivityTypeCode());
		copyAward.setActivityType(orginalAward.getActivityType());
		copyAward.setSequenceNumber(1);
		copyAward.setAwardNumber(awardService.generateNextAwardNumber());
		copyAward.setAwardDocumentTypeCode(Constants.AWARD_SETUP);
		copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(Constants.AWARD_SETUP));
		copyAward.setAwardExecutionDate(orginalAward.getAwardExecutionDate());
		copyAward.setAwardEffectiveDate(orginalAward.getAwardEffectiveDate());
		copyAward.setFundCenter(orginalAward.getFundCenter());
		copyAward.setGrantHeaderId(orginalAward.getGrantHeaderId());
		if (orginalAward.getGrantHeaderId() != null) {
			copyAward.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(orginalAward.getGrantHeaderId()));	
		}
		// active - is false else 
		copyAward = awardDao.saveOrUpdateAwardDetails(copyAward);
		if (vo.getActive().equals(Boolean.FALSE)) {
			copyAward.setIsLatest(false);
			copyAward.setStatusCode(Constants.AWARD_STATUS_CODE_PENDING);
			copyAward.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_PENDING);
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_PENDING));
			copyAward.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT);
			copyAward.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_DRAFT));
		}else {
			copyAward.setIsLatest(true);
			copyAward.setStatusCode(Constants.AWARD_STATUS_CODE_AWARDED);
			copyAward.setAwardSequenceStatus(Constants.AWARD_FINAL_STATUS_ACTIVE);
			copyAward.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_AWARDED));
			copyAward.setWorkflowAwardStatusCode(Constants.AWARD_WORKFLOW_STATUS_ACTIVE);
			copyAward.setAwardWorkflowStatus(awardWorkflowDao.getAwardWorkFlowStatusByCode(Constants.AWARD_WORKFLOW_STATUS_ACTIVE));
			copyAward.setAwardDocumentTypeCode(orginalAward.getAwardDocumentTypeCode());
			copyAward.setAwardDocumentType(awardDao.fetchAwardDocumentTypeById(orginalAward.getAwardDocumentTypeCode()));
			List<CustomData> customDatas = customDataElementDao.fetchCustomDataByParams(Constants.AWARD_MODULE_CODE, orginalAward.getAwardId(), Constants.AWARD_SUBMODULE_CODE , Constants.SUBMODULE_ITEM_KEY);
			if (customDatas != null && !customDatas.isEmpty()) {
				copyCustomDatas(copyAward.getAwardId(), customDatas, vo.getUpdateUser());
			}
			copyQuestionnaireDatas(orginalAward.getAwardId(), vo, copyAward.getAwardId().toString());
		}
		copyAward = awardDao.saveOrUpdateAwardDetails(copyAward);
//		vo.setAward(copyAward);
		List<AwardFundingProposal> newFundingProposals = new ArrayList<>(); 
		List<AwardFundingProposal> fundingProposals = awardDao.getAwardFundingProposals(orginalAward.getAwardId());
		for (AwardFundingProposal proposal : fundingProposals) {
			AwardFundingProposal awardFundingProposal = new AwardFundingProposal();
			awardFundingProposal.setAwardId(copyAward.getAwardId());
			awardFundingProposal.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardFundingProposal.setIsActive(true);
			awardFundingProposal.setProposal(proposal.getProposal());
			awardFundingProposal.setProposalId(proposal.getProposalId());
			try {
				awardFundingProposal = awardDao.saveOrUpdateFundingProposal(awardFundingProposal);
			} catch (Exception e) {
				logger.info("Error occured in copyAwardMandatoryFields : {}", e.getMessage());
			}
			newFundingProposals.add(awardFundingProposal);
		}
	}

	private void copyAwardNonMandatoryFields(IntegrationVO vo, Award copyAward, Award orginalAward) {
		List<AwardPerson> awardPersons = awardDao.getAwardPersonList(orginalAward.getAwardId());
		if (!awardPersons.isEmpty() && awardPersons != null) {
//			copyAwardPersons(copyAward, vo, awardPersons);
			copyAward.setAwardPersons(copyAwardPersons(copyAward, vo, awardPersons));
		}

		List<AwardContact> awardContacts = awardDao.getAwardContactList(orginalAward.getAwardId());
		if (awardContacts != null && !awardContacts.isEmpty()) {
			copyAwardContacts(copyAward, awardContacts, vo.getUpdateUser());
		}
	
		List<AwardProjectTeam> awardProjectTeams = awardDao.getAwardProjectTeamList(orginalAward.getAwardId());
		if (awardProjectTeams != null && !awardProjectTeams.isEmpty()) {
			copyAwardProjectTeams(copyAward, awardProjectTeams, vo.getUpdateUser());
		}
	
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRoles(orginalAward.getAwardId());
		if (awardPersonRoles != null && !awardPersonRoles.isEmpty()) {
			copyAwardPersonRoles(copyAward, awardPersonRoles, vo);
		}

		List<AwardAttachment> awardAttachments = awardDao.getAwardAttachmentsByAwardId(orginalAward.getAwardId(), Boolean.TRUE);
		if (awardAttachments != null && !awardAttachments.isEmpty()) {
			copyAwardAttachments(copyAward, awardAttachments, vo.getUpdateUser());
		}

		if (orginalAward.getAwardKeywords() != null && !orginalAward.getAwardKeywords().isEmpty()) {
			copyAwardKeywords(copyAward, orginalAward, vo.getUpdateUser());
		}

		List<AwardSpecialReview> awardSpecialReview = awardDao.getAwardSpecialReviewsByAwardId(orginalAward.getAwardId());
		if (awardSpecialReview != null && !awardSpecialReview.isEmpty()) {
			copyAwardSpecialReview(copyAward, awardSpecialReview, vo.getUpdateUser());
		}

		List<AwardSubContract> awardSubContracts = awardDao.getSubContractsByAwardId(orginalAward.getAwardId());
		if (awardSubContracts != null && !awardSubContracts.isEmpty()) {
			copyAwardSubContract(copyAward, awardSubContracts, vo.getUpdateUser());
		}

		List<AwardResearchArea> awardResearchAreas = awardDao.fetchAwardResearchAreaBasedOnAwardId(orginalAward.getAwardId());
		if (!awardResearchAreas.isEmpty() && awardResearchAreas != null) {
			copyAwardResearchAreas(copyAward, vo.getUpdateUser(), awardResearchAreas);
		}

		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(orginalAward.getAwardId());
		if (awardCostShares != null && !awardCostShares.isEmpty()) {
			copyAwardCostShare(copyAward, awardCostShares, vo.getUpdateUser());
		}

		List<AwardReportTerms> awardReportTerms = awardDao.getAwardReportTermsByAwardId(orginalAward.getAwardId());
		if (awardReportTerms != null && !awardReportTerms.isEmpty()) {
			copyAwardReportTerms(copyAward, awardReportTerms, vo.getUpdateUser());
		}

		List<AwardSponsorTerm> awardSponsorTerms = awardDao.getAwardSponsorTermByAwardId(orginalAward.getAwardId());
		if (awardSponsorTerms != null && !awardSponsorTerms.isEmpty()) {
			copyAwardSponsorTerm(copyAward, awardSponsorTerms, vo.getUpdateUser());
		}

		List<AwardPublications> awardPublications = awardProjectOutcomeDao.fetchAllAwardPublications(orginalAward.getAwardId());
		if (awardPublications != null && !awardPublications.isEmpty()) {
			copyAwardPublications(copyAward, awardPublications, vo.getUpdateUser());
		}

		List<AwardAssociation> awardAssociations = awardProjectOutcomeDao.fetchAllAwardAssociation(orginalAward.getAwardId());
		if (awardAssociations != null && !awardAssociations.isEmpty()) {
			copyAwardAssociation(copyAward, awardAssociations, vo.getUpdateUser());
		}

		List<AwardAcheivements> awardAcheivements = awardProjectOutcomeDao.fetchAllAwardAcheivements(orginalAward.getAwardId());
		if (awardAcheivements != null && !awardAcheivements.isEmpty()) {
			copyAwardAcheivements(copyAward, awardAcheivements, vo.getUpdateUser());
		}

		List<AwardApprovedEquipment> awardApprovedEquipments = awardDao.getAwardApprovedEquipmentByAwardId(orginalAward.getAwardId());
		if (awardApprovedEquipments != null && !awardApprovedEquipments.isEmpty()) {
			copyAwardApprovedEquipment(copyAward, awardApprovedEquipments, vo.getUpdateUser());
		}

		List<AwardAprovedForeignTravel> awardAprovedForeignTravel = awardDao.getAwardAprovedForeignTravelByAwardId(orginalAward.getAwardId());
		if (awardAprovedForeignTravel != null && !awardAprovedForeignTravel.isEmpty()) {
			copyAwardAprovedForeignTravel(copyAward, awardAprovedForeignTravel, vo.getUpdateUser());
		}

		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_KEY_PERFORMANCE_INDICATOR)) {
			List<AwardKPI> awardKPIs = awardDao.fetchAllAwardKPI(orginalAward.getAwardId());
			if (awardKPIs != null && !awardKPIs.isEmpty()) {
				copyAwardKPIs(copyAward, awardKPIs, vo.getUpdateUser());
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_MILESTONE)) {
			List<AwardMileStone> awardMileStones = awardDao.fetchAwardMileStonesBasedOnAwardId(orginalAward.getAwardId());
			if (awardMileStones != null && !awardMileStones.isEmpty()) {
				copyAwardMileStones(copyAward, awardMileStones, vo.getUpdateUser());
			}
		}
	}

	private List<AwardResearchArea> copyAwardResearchAreas(Award award, String updateUser, List<AwardResearchArea> awardResearchAreas) {
		List<AwardResearchArea> newAwardResearchAreas = new ArrayList<>();
		for (AwardResearchArea copiedAwardResearchArea : awardResearchAreas) {
			AwardResearchArea awardResearchArea = new AwardResearchArea();
			awardResearchArea.setAwardId(award.getAwardId());
			awardResearchArea.setAwardNumber(award.getAwardNumber());
			awardResearchArea.setSequenceNumber(award.getSequenceNumber());
			awardResearchArea.setResearchTypeCode(copiedAwardResearchArea.getResearchTypeCode());
			awardResearchArea.setResearchTypeAreaCode(copiedAwardResearchArea.getResearchTypeAreaCode());
			awardResearchArea.setResearchTypeArea(copiedAwardResearchArea.getResearchTypeArea());
			awardResearchArea.setResearchType(copiedAwardResearchArea.getResearchType());
			awardResearchArea.setResearchTypeSubAreaCode(copiedAwardResearchArea.getResearchTypeSubAreaCode());
			awardResearchArea.setResearchTypeSubArea(copiedAwardResearchArea.getResearchTypeSubArea());
			awardResearchArea.setUpdateUser(updateUser);
			awardResearchArea.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardResearchArea = awardDao.saveOrUpdateAwardResearchArea(awardResearchArea);
			newAwardResearchAreas.add(awardResearchArea);
		}
		return newAwardResearchAreas;
	}

	private void copyAwardPersonRoles(Award copyAward, List<AwardPersonRoles> awardPersonRoles, IntegrationVO vo) {
		List<AwardPersonRoles> newAwardPersonRoles = new ArrayList<>();
		for (AwardPersonRoles copiedAwardPersonRoles : awardPersonRoles) {
			AwardPersonRoles awardPersonRole = new AwardPersonRoles();
			awardPersonRole.setAwardId(copyAward.getAwardId());
			awardPersonRole.setAwardNumber(copyAward.getAwardNumber());
			awardPersonRole.setSequenceNumber(copyAward.getSequenceNumber());
			awardPersonRole.setPersonId(copyAward.getPiPersonId());
			awardPersonRole.setRoleId(copiedAwardPersonRoles.getRoleId());
			awardPersonRole.setIsSystemGenerated(copiedAwardPersonRoles.getIsSystemGenerated());
			awardPersonRole.setUpdateUser(vo.getUpdateUser());
			awardPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardPersonRole = awardDao.saveOrUpdateAwardPersonRoles(awardPersonRole);
			newAwardPersonRoles.add(awardPersonRole);
		}
	}

	private void copyAwardAprovedForeignTravel(Award copyAward,
			List<AwardAprovedForeignTravel> awardAprovedForeignTravels, String updateUser) {
		List<AwardAprovedForeignTravel> newAwardAprovedForeignTravels = new ArrayList<>();
		for (AwardAprovedForeignTravel copiedAwardAprovedForeignTravel : awardAprovedForeignTravels) {
			AwardAprovedForeignTravel awardAprovedForeignTravel = new AwardAprovedForeignTravel();
			awardAprovedForeignTravel.setAwardId(copyAward.getAwardId());
			awardAprovedForeignTravel.setAwardNumber(copyAward.getAwardNumber());
			awardAprovedForeignTravel.setSequenceNumber(copyAward.getSequenceNumber());
			awardAprovedForeignTravel.setPersonId(copiedAwardAprovedForeignTravel.getPersonId());
			awardAprovedForeignTravel.setRolodexId(copiedAwardAprovedForeignTravel.getRolodexId());
			awardAprovedForeignTravel.setTravellerName(copiedAwardAprovedForeignTravel.getTravellerName());
			awardAprovedForeignTravel.setDestination(copiedAwardAprovedForeignTravel.getDestination());
			awardAprovedForeignTravel.setStartDate(copiedAwardAprovedForeignTravel.getStartDate());
			awardAprovedForeignTravel.setEndDate(copiedAwardAprovedForeignTravel.getEndDate());
			awardAprovedForeignTravel.setAmount(copiedAwardAprovedForeignTravel.getAmount());
			awardAprovedForeignTravel.setUpdateUser(updateUser);
			awardAprovedForeignTravel.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			try {
				awardAprovedForeignTravel = awardDao.saveOrUpdateAwardAprovedForeignTravel(awardAprovedForeignTravel);
			} catch (Exception e) {
				logger.info("Error occured in copyAwardAprovedForeignTravel : {}", e.getMessage());
			}
			newAwardAprovedForeignTravels.add(awardAprovedForeignTravel);
		}
	}

	private void copyAwardApprovedEquipment(Award copyAward, List<AwardApprovedEquipment> awardApprovedEquipments,
			String updateUser) {
		List<AwardApprovedEquipment> newAwardApprovedEquipment = new ArrayList<>();
		for (AwardApprovedEquipment copiedAwardApprovedEquipment : awardApprovedEquipments) {
			AwardApprovedEquipment awardApprovedEquipment = new AwardApprovedEquipment();
			awardApprovedEquipment.setAwardId(copyAward.getAwardId());
			awardApprovedEquipment.setAwardNumber(copyAward.getAwardNumber());
			awardApprovedEquipment.setSequenceNumber(copyAward.getSequenceNumber());
			awardApprovedEquipment.setItem(copiedAwardApprovedEquipment.getItem());
			awardApprovedEquipment.setModel(copiedAwardApprovedEquipment.getModel());
			awardApprovedEquipment.setVendor(copiedAwardApprovedEquipment.getVendor());
			awardApprovedEquipment.setAmount(copiedAwardApprovedEquipment.getAmount());
			awardApprovedEquipment.setUpdateUser(updateUser);
			awardApprovedEquipment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			try {
				awardApprovedEquipment = awardDao.saveOrUpdateAwardApprovedEquipment(awardApprovedEquipment);
			} catch (Exception e) {
				logger.info("Error occured in copyAwardApprovedEquipment : {}", e.getMessage());
			}
			newAwardApprovedEquipment.add(awardApprovedEquipment);
		}
	}

	private void copyAwardAcheivements(Award copyAward, List<AwardAcheivements> awardAcheivements, String updateUser) {
		List<AwardAcheivements> newAwardAcheivements = new ArrayList<>();
		for (AwardAcheivements copiedAwardAcheivements : awardAcheivements) {
			AwardAcheivements awardAcheivement = new AwardAcheivements();
			awardAcheivement.setAwardId(copyAward.getAwardId());
			awardAcheivement.setAwardNumber(copyAward.getAwardNumber());
			awardAcheivement.setSequenceNumber(copyAward.getSequenceNumber());
			awardAcheivement.setFileName(copiedAwardAcheivements.getFileName());
			awardAcheivement.setMimeType(copiedAwardAcheivements.getMimeType());
			awardAcheivement.setComment(copiedAwardAcheivements.getComment());
			if (copiedAwardAcheivements.getFileDataId() != null) {
				FileData fileData = commonDao.getFileDataById(copiedAwardAcheivements.getFileDataId());
				FileData dataCopy = commonDao.saveFileData(fileData);
				awardAcheivement.setFileDataId(dataCopy.getFileDataId());
			}
			awardAcheivement.setUpdateUser(updateUser);
			awardAcheivement.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardProjectOutcomeDao.saveOrUpdateAwardAcheivements(awardAcheivement);
			newAwardAcheivements.add(awardAcheivement);
		}	
	}

	private void copyAwardAssociation(Award copyAward, List<AwardAssociation> awardAssociations, String updateUser) {	
		List<AwardAssociation> newAwardAssociations = new ArrayList<>();	
		for (AwardAssociation copiedAwardAssociation : awardAssociations) {	
			AwardAssociation awardAssociation = new AwardAssociation();	
			awardAssociation.setAwardId(copyAward.getAwardId());	
			awardAssociation.setAwardNumber(copyAward.getAwardNumber());	
			awardAssociation.setSequenceNumber(copyAward.getSequenceNumber());	
			awardAssociation.setAssociationTypeCode(copiedAwardAssociation.getAssociationTypeCode());	
			awardAssociation.setAwardAssociationType(copiedAwardAssociation.getAwardAssociationType());	
			awardAssociation.setAssociatedProjectId(copiedAwardAssociation.getAssociatedProjectId());	
			awardAssociation.setUpdateUser(updateUser);	
			awardAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());	
			awardProjectOutcomeDao.saveOrUpdateAwardAssociation(awardAssociation);	
			if (copiedAwardAssociation.getAssociationTypeCode().equals(Constants.EXTERNAL_ASSOCIATION_TYPE)) {	
				AwardAssociationDetail copiedAwardAssociationDetail = copiedAwardAssociation.getAwardAssociationDetail();	
				AwardAssociationDetail awardAssociationDetail = new AwardAssociationDetail();	
				awardAssociation.setAwardAssociationDetail(copiedAwardAssociation.getAwardAssociationDetail());	
				awardAssociationDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());	
				awardAssociationDetail.setAwardAssociation(awardAssociation);	
				awardAssociationDetail.setTitle(copiedAwardAssociationDetail.getTitle());	
				awardAssociationDetail.setSponsorCode(copiedAwardAssociationDetail.getSponsorCode());	
				awardAssociationDetail.setContactAdminPersonId(copiedAwardAssociationDetail.getContactAdminPersonId());	
				awardAssociationDetail.setLeadUnit(copiedAwardAssociationDetail.getLeadUnit());	
				awardAssociationDetail.setPiName(copiedAwardAssociationDetail.getPiName());	
				awardAssociationDetail.setPrimeSponsorCode(copiedAwardAssociationDetail.getPrimeSponsorCode());	
				awardAssociationDetail.setRolodexId(copiedAwardAssociationDetail.getRolodexId());	
				awardAssociationDetail.setUpdateUser(updateUser);	
				awardAssociationDetail.setSponsorAwardNumber(copiedAwardAssociationDetail.getSponsorAwardNumber());					
				awardProjectOutcomeDao.saveAwardAssociationDetail(awardAssociationDetail);	
				newAwardAssociations.add(awardAssociation);	
			}	
		}		
	}

	private void copyAwardPublications(Award copyAward, List<AwardPublications> awardPublications, String updateUser) {
		List<AwardPublications> newAwardPublications = new ArrayList<>();
		for (AwardPublications copiedAwardPublication : awardPublications) {
			AwardPublications awardPublication = new AwardPublications();
			awardPublication.setAwardId(copyAward.getAwardId());
			awardPublication.setAwardNumber(copyAward.getAwardNumber());
			awardPublication.setSequenceNumber(copyAward.getSequenceNumber());
			awardPublication.setPublicationId(copiedAwardPublication.getPublicationId());
			awardPublication.setPublication(copiedAwardPublication.getPublication());
			awardPublication.setUpdateUser(updateUser);
			awardPublication.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardProjectOutcomeDao.saveOrUpdateAwardPublications(awardPublication);
			newAwardPublications.add(awardPublication);
		}
	}

	private void copyAwardSponsorTerm(Award copyAward, List<AwardSponsorTerm> awardSponsorTerms, String updateUser) {
		List<AwardSponsorTerm> newAwardSponsorTerm = new ArrayList<>();
		for (AwardSponsorTerm copiedAwardSponsorTerm : awardSponsorTerms) {
			AwardSponsorTerm awardSponsorTerm = new AwardSponsorTerm();
			awardSponsorTerm.setAwardId(copyAward.getAwardId());
			awardSponsorTerm.setAward(copyAward);
			awardSponsorTerm.setAwardNumber(copyAward.getAwardNumber());
			awardSponsorTerm.setSequenceNumber(copyAward.getSequenceNumber());
			awardSponsorTerm.setSponsorTermTypeCode(copiedAwardSponsorTerm.getSponsorTermTypeCode());
			awardSponsorTerm.setSponsorTermCode(copiedAwardSponsorTerm.getSponsorTermCode());
			awardSponsorTerm.setUpdateUser(updateUser);
			awardSponsorTerm.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardSponsorTerms(awardSponsorTerm);
			newAwardSponsorTerm.add(awardSponsorTerm);
		}
	}

	private void copyAwardReportTerms(Award copyAward, List<AwardReportTerms> awardReportTerms, String updateUser) {
		List<AwardReportTerms> newAwardReportTerms = new ArrayList<>();
		for (AwardReportTerms copiedAwardReportTerms : awardReportTerms) {
			AwardReportTerms awardReportTerm = new AwardReportTerms();
			awardReportTerm.setAwardId(copyAward.getAwardId());
			awardReportTerm.setAward(copyAward);
			awardReportTerm.setAwardNumber(copyAward.getAwardNumber());
			awardReportTerm.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTerm.setReportClassCode(copiedAwardReportTerms.getReportClassCode());
			awardReportTerm.setReportCode(copiedAwardReportTerms.getReportCode());
			awardReportTerm.setFrequencyCode(copiedAwardReportTerms.getFrequencyCode());
			awardReportTerm.setFrequencyBaseCode(copiedAwardReportTerms.getFrequencyBaseCode());
			awardReportTerm.setOspDistributionCode(copiedAwardReportTerms.getOspDistributionCode());
			awardReportTerm.setDueDate(copiedAwardReportTerms.getDueDate());
			awardReportTerm.setUpdateUser(updateUser);
			awardReportTerm.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			List<AwardReportTermRecipient> awardReportTermRecipient = copiedAwardReportTerms.getAwardReportTermRecipient();
			if (awardReportTermRecipient != null && !awardReportTermRecipient.isEmpty()) {
				awardReportTerm.getAwardReportTermRecipient().addAll(copyAwardReportTermRecipient(copyAward, awardReportTerm, awardReportTermRecipient, updateUser));
			}
			List<AwardReportTracking> awardReportTracking = copiedAwardReportTerms.getAwardReportTracking();
			if (awardReportTracking != null && !awardReportTracking.isEmpty()) {
				awardReportTerm.getAwardReportTracking().addAll(copyAwardReportTracking(copyAward, awardReportTerm, awardReportTracking, updateUser));
			}
			awardDao.saveOrUpdateAwardReports(awardReportTerm);
			newAwardReportTerms.add(awardReportTerm);
		}
	}

	private List<AwardReportTracking> copyAwardReportTracking(Award copyAward, AwardReportTerms copiedAwardReportTerms,  List<AwardReportTracking> awardReportTrackings, String updateUser) {
		List<AwardReportTracking> newAwardReportTrackings = new ArrayList<>();
		for (AwardReportTracking copiedAwardReportTracking : awardReportTrackings) {
			AwardReportTracking awardReportTracking = new AwardReportTracking();
			awardReportTracking.setAwardId(copyAward.getAwardId());
			awardReportTracking.setAwardNumber(copyAward.getAwardNumber());
			awardReportTracking.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTracking.setAwardReportTerms(copiedAwardReportTerms);
			awardReportTracking.setStatusCode(copiedAwardReportTracking.getStatusCode());
			awardReportTracking.setActivityDate(copiedAwardReportTracking.getActivityDate());
			awardReportTracking.setComments(copiedAwardReportTracking.getComments());
			awardReportTracking.setPreparerId(copiedAwardReportTracking.getPreparerId());
			awardReportTracking.setCreateUser(copiedAwardReportTracking.getCreateUser());
			awardReportTracking.setCreateDate(copiedAwardReportTracking.getCreateDate());
			awardReportTracking.setDueDate(copiedAwardReportTracking.getDueDate());
			awardReportTracking.setPreparerName(copiedAwardReportTracking.getPreparerName());
			awardReportTracking.setUpdateUser(updateUser);
			awardReportTracking.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAwardReportTrackings.add(awardReportTracking);
		}
		return newAwardReportTrackings;
	}

	private List<AwardReportTermRecipient> copyAwardReportTermRecipient(Award copyAward, AwardReportTerms copiedAwardReportTerms, List<AwardReportTermRecipient> copiedawardReportTermRecipient, String updateUser) {
		List<AwardReportTermRecipient> newAwardReportTermRecipients = new ArrayList<>();
		for (AwardReportTermRecipient copiedAwardReportTermRecipient : copiedawardReportTermRecipient) {
			AwardReportTermRecipient awardReportTermRecipient = new AwardReportTermRecipient();
			awardReportTermRecipient.setAwardId(copyAward.getAwardId());
			awardReportTermRecipient.setAwardNumber(copyAward.getAwardNumber());
			awardReportTermRecipient.setSequenceNumber(copyAward.getSequenceNumber());
			awardReportTermRecipient.setAwardReportTerms(copiedAwardReportTerms);
			awardReportTermRecipient.setRecipientId(copiedAwardReportTermRecipient.getRecipientId());
			awardReportTermRecipient.setUpdateUser(updateUser);
			awardReportTermRecipient.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAwardReportTermRecipients.add(awardReportTermRecipient);
		}
		return newAwardReportTermRecipients;
	}

//	private void copyAwardAmountInfo(Award copyAward, List<AwardAmountInfo> awardAmountInfos, String updateUser) {
//		List<AwardAmountInfo> newAwardAmountInfos = new ArrayList<>();
//		for (AwardAmountInfo copiedAwardAmountInfoDetail : awardAmountInfos) {
//			AwardAmountInfo awardAmountInfoDetail = new AwardAmountInfo();
//			awardAmountInfoDetail.setAwardId(copyAward.getAwardId());
//			awardAmountInfoDetail.setAwardNumber(copyAward.getAwardNumber());
//			awardAmountInfoDetail.setSequenceNumber(copyAward.getSequenceNumber());
//			awardAmountInfoDetail.setTransactionId(copiedAwardAmountInfoDetail.getTransactionId());
//			awardAmountInfoDetail.setAwardAmountTransaction(copiedAwardAmountInfoDetail.getAwardAmountTransaction());
//			awardAmountInfoDetail.setAnticipatedChange(copiedAwardAmountInfoDetail.getAnticipatedChange());
//			awardAmountInfoDetail.setAnticipatedChangeDirect(copiedAwardAmountInfoDetail.getAnticipatedChangeDirect());
//			awardAmountInfoDetail.setAnticipatedChangeIndirect(copiedAwardAmountInfoDetail.getAnticipatedChangeIndirect());
//			awardAmountInfoDetail.setObligatedChange(copiedAwardAmountInfoDetail.getObligatedChange());
//			awardAmountInfoDetail.setObligatedChangeDirect(copiedAwardAmountInfoDetail.getObligatedChangeDirect());
//			awardAmountInfoDetail.setObligatedChangeIndirect(copiedAwardAmountInfoDetail.getObligatedChangeIndirect());
//			awardAmountInfoDetail.setAnticipatedTotalDirect(copiedAwardAmountInfoDetail.getAnticipatedTotalDirect());
//			awardAmountInfoDetail.setAnticipatedChangeIndirect(copiedAwardAmountInfoDetail.getAnticipatedChangeIndirect());
//			awardAmountInfoDetail.setAnticipatedTotalAmount(copiedAwardAmountInfoDetail.getAnticipatedTotalAmount());
//			awardAmountInfoDetail.setObligatedTotalDirect(copiedAwardAmountInfoDetail.getObligatedTotalDirect());
//			awardAmountInfoDetail.setObligatedTotalIndirect(copiedAwardAmountInfoDetail.getObligatedTotalIndirect());
//			awardAmountInfoDetail.setAmountObligatedToDate(copiedAwardAmountInfoDetail.getAmountObligatedToDate());
//			awardAmountInfoDetail.setAntDistributableAmount(copiedAwardAmountInfoDetail.getAntDistributableAmount());
//			awardAmountInfoDetail.setObliDistributableAmount(copiedAwardAmountInfoDetail.getObliDistributableAmount());
//			awardAmountInfoDetail.setFinalExpirationDate(copiedAwardAmountInfoDetail.getFinalExpirationDate());
//			awardAmountInfoDetail.setCurrentFundEffectiveDate(copiedAwardAmountInfoDetail.getCurrentFundEffectiveDate());
//			awardAmountInfoDetail.setObligationExpirationDate(copiedAwardAmountInfoDetail.getObligationExpirationDate());
//			awardAmountInfoDetail.setUpdateUser(updateUser);
//			awardAmountInfoDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
//			datesAndAmountDao.saveOrUpdateAwardAmountInfo(awardAmountInfoDetail);
//			newAwardAmountInfos.add(awardAmountInfoDetail);
//		}
//	}

	private List<AwardSpecialReview> copyAwardSpecialReview(Award copyAward, List<AwardSpecialReview> awardSpecialReview, String updateUser) {
		List<AwardSpecialReview> newAwardSpecialReviews = new ArrayList<>();
		for (AwardSpecialReview copiedAwardSpecialReviewDetail : awardSpecialReview) {
			AwardSpecialReview awardSpecialReviewDetail = new AwardSpecialReview();
			awardSpecialReviewDetail.setAwardId(copyAward.getAwardId());
			awardSpecialReviewDetail.setAwardNumber(copyAward.getAwardNumber());
			awardSpecialReviewDetail.setSpecialReviewCode(copiedAwardSpecialReviewDetail.getSpecialReviewCode());
			awardSpecialReviewDetail.setSpecialReview(copiedAwardSpecialReviewDetail.getSpecialReview());
			awardSpecialReviewDetail.setApprovalTypeCode(copiedAwardSpecialReviewDetail.getApprovalTypeCode());
			awardSpecialReviewDetail.setExpirationDate(copiedAwardSpecialReviewDetail.getExpirationDate());
			awardSpecialReviewDetail.setProtocolNumber(copiedAwardSpecialReviewDetail.getProtocolNumber());
			awardSpecialReviewDetail.setApplicationDate(copiedAwardSpecialReviewDetail.getApplicationDate());
			awardSpecialReviewDetail.setApprovalDate(copiedAwardSpecialReviewDetail.getApprovalDate());
			awardSpecialReviewDetail.setComments(copiedAwardSpecialReviewDetail.getComments());
			awardSpecialReviewDetail.setUpdateUser(updateUser);
			awardSpecialReviewDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardSpecialReview(awardSpecialReviewDetail);
			newAwardSpecialReviews.add(awardSpecialReviewDetail);
		}
		return newAwardSpecialReviews;
	}

	private List<AwardCostShare> copyAwardCostShare(Award copyAward, List<AwardCostShare> awardCostShares, String updateUser) {
		List<AwardCostShare> newAwardCostShares = new ArrayList<>();
		for (AwardCostShare copiedAwardCostShareDetail : awardCostShares) {
			AwardCostShare awardCostShareDetail = new AwardCostShare();
			awardCostShareDetail.setAwardId(copyAward.getAwardId());
			awardCostShareDetail.setAwardNumber(copyAward.getAwardNumber());
			awardCostShareDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardCostShareDetail.setProjectPeriod(copiedAwardCostShareDetail.getProjectPeriod());
			awardCostShareDetail.setCostSharePercentage(copiedAwardCostShareDetail.getCostSharePercentage());
			awardCostShareDetail.setCostShareTypeCode(copiedAwardCostShareDetail.getCostShareTypeCode());
			awardCostShareDetail.setSource(copiedAwardCostShareDetail.getSource());
			awardCostShareDetail.setDestination(copiedAwardCostShareDetail.getDestination());
			awardCostShareDetail.setCommitmentAmount(copiedAwardCostShareDetail.getCommitmentAmount());
			awardCostShareDetail.setVerificationDate(copiedAwardCostShareDetail.getVerificationDate());
			awardCostShareDetail.setCostShareMet(copiedAwardCostShareDetail.getCostShareMet());
			awardCostShareDetail.setUnitNumber(copiedAwardCostShareDetail.getUnitNumber());
			awardCostShareDetail.setUpdateUser(updateUser);
			awardCostShareDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardCostShare(awardCostShareDetail);
			newAwardCostShares.add(awardCostShareDetail);
		}
		return newAwardCostShares;
	}

	private List<AwardSubContract> copyAwardSubContract(Award copyAward, List<AwardSubContract> awardSubContracts, String updateUser) {
		List<AwardSubContract> newAwardSubContracts = new ArrayList<>();
		for (AwardSubContract copiedAwardSubContractDetail : awardSubContracts) {
			AwardSubContract awardSubContractDetail = new AwardSubContract();
			awardSubContractDetail.setAwardId(copyAward.getAwardId());
			awardSubContractDetail.setAwardNumber(copyAward.getAwardNumber());
			awardSubContractDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardSubContractDetail.setOrganizationId(copiedAwardSubContractDetail.getOrganizationId());
			awardSubContractDetail.setOrganization(copiedAwardSubContractDetail.getOrganization());
			awardSubContractDetail.setAmount(copiedAwardSubContractDetail.getAmount());
			awardSubContractDetail.setUpdateUser(updateUser);
			awardSubContractDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardSubContract(awardSubContractDetail);
			newAwardSubContracts.add(awardSubContractDetail);
		}
		return newAwardSubContracts;
	}

	private List<AwardContact> copyAwardContacts(Award copyAward, List<AwardContact> awardContacts, String updateUser) {
		List<AwardContact> newAwardContacts = new ArrayList<>();
		for (AwardContact copiedAwardContactDetail : awardContacts) {
			AwardContact awardContactDetail = new AwardContact();
			awardContactDetail.setAwardId(copyAward.getAwardId());
			awardContactDetail.setAwardNumber(copyAward.getAwardNumber());
			awardContactDetail.setSequenceNumber(copyAward.getSequenceNumber());
			awardContactDetail.setRolodexId(copiedAwardContactDetail.getRolodexId());
			awardContactDetail.setRolodex(copiedAwardContactDetail.getRolodex());
			awardContactDetail.setFullName(copiedAwardContactDetail.getFullName());
			awardContactDetail.setAwardContactType(copiedAwardContactDetail.getAwardContactType());
			awardContactDetail.setContactTypeCode(copiedAwardContactDetail.getContactTypeCode());
			awardContactDetail.setUpdateUser(updateUser);
			awardContactDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardContact(awardContactDetail);
			newAwardContacts.add(awardContactDetail);
		}
		return newAwardContacts;
	}

	private List<AwardKeyword> copyAwardKeywords(Award copyAward, Award orginalAward, String updateUser) {
		List<AwardKeyword> awardKeywords = orginalAward.getAwardKeywords();
		List<AwardKeyword> copiedAwardKeywords = new ArrayList<>(awardKeywords);
		Collections.copy(copiedAwardKeywords, awardKeywords);
		List<AwardKeyword> newKeywords = new ArrayList<>();
		for (AwardKeyword copiedKeywordDetail : copiedAwardKeywords) {
			AwardKeyword keywordDetail = new AwardKeyword();
			keywordDetail.setAward(copyAward);
			keywordDetail.setScienceKeywordCode(copiedKeywordDetail.getScienceKeywordCode());
			keywordDetail.setScienceKeyword(copiedKeywordDetail.getScienceKeyword());
			keywordDetail.setUpdateUser(updateUser);
			keywordDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newKeywords.add(keywordDetail);
		}
		return newKeywords;
	}

	private List<AwardProjectTeam> copyAwardProjectTeams(Award copyAward, List<AwardProjectTeam> awardProjectTeams, String updateUser) {
		List<AwardProjectTeam> newAwardProjectTeams = new ArrayList<>();
		for (AwardProjectTeam copiedProjectTeamDetail : awardProjectTeams) {
			AwardProjectTeam projectTeamDetail = new AwardProjectTeam();
			projectTeamDetail.setAwardId(copyAward.getAwardId());
			projectTeamDetail.setAwardNumber(copyAward.getAwardNumber());
			projectTeamDetail.setSequenceNumber(copyAward.getSequenceNumber());
			projectTeamDetail.setPersonId(copiedProjectTeamDetail.getPersonId());
			projectTeamDetail.setFullName(copiedProjectTeamDetail.getFullName());
			projectTeamDetail.setProjectRole(copiedProjectTeamDetail.getProjectRole());
			projectTeamDetail.setNonEmployeeFlag(copiedProjectTeamDetail.getNonEmployeeFlag());
			projectTeamDetail.setPercentageCharged(copiedProjectTeamDetail.getPercentageCharged());
			projectTeamDetail.setStartDate(copiedProjectTeamDetail.getStartDate());
			projectTeamDetail.setEndDate(copiedProjectTeamDetail.getEndDate());
			projectTeamDetail.setIsActive(copiedProjectTeamDetail.getIsActive());
			projectTeamDetail.setUpdateUser(updateUser);
			projectTeamDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardProjectTeam(projectTeamDetail);
			newAwardProjectTeams.add(projectTeamDetail);
		}
		return newAwardProjectTeams;
	}

	private List<AwardAttachment> copyAwardAttachments(Award copyAward, List<AwardAttachment> awardAttachments, String updateUser) {
		List<AwardAttachment> copiedAwardAttachments = new ArrayList<>(awardAttachments);
		Collections.copy(copiedAwardAttachments, awardAttachments);
		List<AwardAttachment> newAttachments = new ArrayList<>();
		for (AwardAttachment copiedAttachmentDetail : copiedAwardAttachments) {
			AwardAttachment attachmentDetail = new AwardAttachment();
			attachmentDetail.setAwardId(copyAward.getAwardId());
			attachmentDetail.setAwardNumber(copyAward.getAwardNumber());
			attachmentDetail.setSequenceNumber(copyAward.getSequenceNumber());
			attachmentDetail.setDescription(copiedAttachmentDetail.getDescription());
			attachmentDetail.setFileName(copiedAttachmentDetail.getFileName());
			attachmentDetail.setTypeCode(copiedAttachmentDetail.getTypeCode());
			attachmentDetail.setMimeType(copiedAttachmentDetail.getMimeType());
			attachmentDetail.setUpdateUser(updateUser);
			attachmentDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			attachmentDetail.setNarrativeStatusCode(copiedAttachmentDetail.getNarrativeStatusCode());
			attachmentDetail.setNarrativeStatus(copiedAttachmentDetail.getNarrativeStatus());
			attachmentDetail.setVersionNumber(copiedAttachmentDetail.getVersionNumber());
			attachmentDetail.setDocumentStatusCode(copiedAttachmentDetail.getDocumentStatusCode());
			attachmentDetail.setAttachmentType(copiedAttachmentDetail.getAttachmentType());
			attachmentDetail.setDescription(copiedAttachmentDetail.getDescription());
			FileData fileData = commonDao.getFileDataById(copiedAttachmentDetail.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			attachmentDetail.setFileDataId(file.getFileDataId());
			attachmentDetail.setDocumentId(copiedAttachmentDetail.getDocumentId());
			awardDao.saveAttachment(attachmentDetail);
			newAttachments.add(attachmentDetail);
		}
		return newAttachments;
	}

	private List<AwardPerson> copyAwardPersons(Award copyAward, IntegrationVO vo, List<AwardPerson> awardPersons) {
		List<AwardPerson> newAwardPersons = new ArrayList<>();
		for (AwardPerson copiedPersonDetail : awardPersons) {
			AwardPerson personDetail = new AwardPerson();
			personDetail.setAwardId(copyAward.getAwardId());
			personDetail.setAwardNumber(copyAward.getAwardNumber());
			personDetail.setSequenceNumber(copyAward.getSequenceNumber());
			personDetail.setRolodexId(copiedPersonDetail.getRolodexId());
			personDetail.setPercentageEffort(copiedPersonDetail.getPercentageEffort());
			if(copiedPersonDetail.getIsPi().equals(Boolean.TRUE)) {
				personDetail.setUnitNumber(vo.getUnitNumber());
				personDetail.setUnitName(commonDao.getUnitName(vo.getUnitNumber()));
				Person person = personDao.getPersonDetailById(personDao.getPersonIdByUserName(vo.getPersonName()));
				if (person != null) {
					personDetail.setPersonId(person.getPersonId());
					personDetail.setFullName(person.getFullName());
					personDetail.setEmailAddress(person.getEmailAddress());
				}
			} else {
				personDetail.setUnitNumber(copiedPersonDetail.getUnitNumber());
				personDetail.setUnitName(copiedPersonDetail.getUnitName());
				personDetail.setPersonId(copiedPersonDetail.getPersonId());
				personDetail.setFullName(copiedPersonDetail.getFullName());
				personDetail.setEmailAddress(copiedPersonDetail.getEmailAddress());
			}
			personDetail.setPersonRoleId(copiedPersonDetail.getPersonRoleId());
			personDetail.setDepartment(copiedPersonDetail.getDepartment());
			personDetail.setProjectRole(copiedPersonDetail.getProjectRole());
			personDetail.setUpdateUser(vo.getUpdateUser());
			personDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			personDetail.setProposalPersonRole(copiedPersonDetail.getProposalPersonRole());
			personDetail.setIsPi(copiedPersonDetail.getIsPi());
			personDetail.setDesignation(copiedPersonDetail.getDesignation());
			List<AwardPersonUnit> units = copiedPersonDetail.getAwardPersonUnits();
			if (units != null && !units.isEmpty()) {
				personDetail.getAwardPersonUnits().addAll(copyAwardPersonUnits(copyAward, copiedPersonDetail, personDetail, vo));
			}
			List<AwardPersonAttachment> personAttachments = copiedPersonDetail.getAwardPersonAttachment();
			if (personAttachments != null && !personAttachments.isEmpty()) {
				personDetail.getAwardPersonAttachment().addAll(copyAwardPersonAttachment(copiedPersonDetail, personDetail, vo.getUpdateUser()));
			}
			personDetail.setIsMultiPi(copiedPersonDetail.getIsMultiPi());
			awardDao.saveOrUpdateAwardPersons(personDetail);
			newAwardPersons.add(personDetail);
		}
		return newAwardPersons;
	}

	private List<AwardPersonAttachment> copyAwardPersonAttachment(AwardPerson copiedPersonDetail,
			AwardPerson personDetail, String updateUser) {
		List<AwardPersonAttachment> proposalPersonAttachments = copiedPersonDetail.getAwardPersonAttachment();
		List<AwardPersonAttachment> newproposalPersonAttachments = new ArrayList<>();
		for (AwardPersonAttachment copiedAwardPersonAttachment : proposalPersonAttachments) {
			AwardPersonAttachment attachment = new AwardPersonAttachment();
			attachment.setAwardPerson(personDetail);
			attachment.setDescription(copiedAwardPersonAttachment.getDescription());
			attachment.setFileName(copiedAwardPersonAttachment.getFileName());
			attachment.setMimeType(copiedAwardPersonAttachment.getMimeType());
			FileData fileData = commonDao.getFileDataById(copiedAwardPersonAttachment.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			attachment.setFileDataId(file.getFileDataId());
			attachment.setUpdateUser(updateUser);
			attachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newproposalPersonAttachments.add(attachment);
		}
		return newproposalPersonAttachments;
	}

	private List<AwardPersonUnit> copyAwardPersonUnits(Award copyAward, AwardPerson copiedPersonDetail,
			AwardPerson personDetail, IntegrationVO vo) {
		List<AwardPersonUnit> proposalPersonUnits = copiedPersonDetail.getAwardPersonUnits();
		List<AwardPersonUnit> newAwardPersonUnits = new ArrayList<>();
		for (AwardPersonUnit copiedPersonPersonUnit : proposalPersonUnits) {
			AwardPersonUnit personUnit = new AwardPersonUnit();
			personUnit.setAwardId(copyAward.getAwardId());
			personDetail.setAwardNumber(copyAward.getAwardNumber());
			personDetail.setSequenceNumber(copyAward.getSequenceNumber());
			personUnit.setAwardPerson(personDetail);
			if(personDetail.getIsPi().equals(Boolean.TRUE)) {
				personUnit.setUnitNumber(vo.getUnitNumber());
				personUnit.setLeadUnitFlag(true);
				personUnit.setUnit(commonDao.getUnitByUnitNumber(vo.getUnitNumber()));
			} else {
				personUnit.setUnitNumber(copiedPersonPersonUnit.getUnitNumber());
				personUnit.setLeadUnitFlag(copiedPersonPersonUnit.getLeadUnitFlag());
				personUnit.setUnit(copiedPersonPersonUnit.getUnit());
			}
			personUnit.setUpdateUser(vo.getUpdateUser());
			personUnit.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newAwardPersonUnits.add(personUnit);
		}
		return newAwardPersonUnits;
	}

	public AwardBudgetHeader createAwardBudgetHeader(IntegrationVO vo, Award award, AwardBudgetHeader awardBudgetHeader,
			String serviceRequestTypeCode) {
		AwardBudgetHeader awardBudget = new AwardBudgetHeader();
		awardBudget.setAwardId(award.getAwardId());
		awardBudget.setAwardNumber(award.getAwardNumber());
		awardBudget.setObligatedTotal(awardBudgetHeader.getObligatedTotal());
		awardBudget.setObligatedChange(awardBudgetHeader.getObligatedChange());
		awardBudget.setIsAutoCalc(awardBudgetHeader.getIsAutoCalc());
		awardBudget.setEndDate(awardBudgetHeader.getEndDate());
		awardBudget.setStartDate(awardBudgetHeader.getStartDate());
		awardBudget.setTotalCost(awardBudgetHeader.getTotalCost());
		awardBudget.setTotalDirectCost(awardBudgetHeader.getTotalDirectCost());
		awardBudget.setTotalIndirectCost(awardBudgetHeader.getTotalIndirectCost());
		awardBudget.setComments(awardBudgetHeader.getComments());
		awardBudget.setOnOffCampusFlag(awardBudgetHeader.getOnOffCampusFlag());
		awardBudget.setRateClassCode(awardBudgetHeader.getRateClassCode());
		awardBudget.setRateTypeCode(awardBudgetHeader.getRateTypeCode());
		awardBudget.setRateType(awardBudgetHeader.getRateType());
		awardBudget.setAnticipatedTotal(awardBudgetHeader.getAnticipatedTotal());
		awardBudget.setFundCode(award.getAccountNumber());
		awardBudget.setFundCenter(award.getFundCenter());
		if (serviceRequestTypeCode.equals(Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE)) {
			awardBudget.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS));
			awardBudget.setBudgetStatusCode(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS);
			Integer maxBudgetVersionNumber = awardBudgetDao.maxAwardBudgetVersionNumberByAwardId(awardBudgetHeader.getAwardId());
			logger.info("maxBudgetVersionNumber : {}", maxBudgetVersionNumber);
			awardBudget.setVersionNumber(awardBudgetHeader.getVersionNumber() + 1);
			awardBudget.setSequenceNumber(awardBudgetHeader.getSequenceNumber() + 1);
			awardBudget.setBudgetTypeCode(Constants.AWARD_BUDGET_TYPE_CODE_REBUDGET);
			awardBudget.setBudgetType(awardBudgetDao.getBudgetTypeById(Constants.AWARD_BUDGET_TYPE_CODE_REBUDGET));
			awardBudget.setCreateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudget.setCreateUser(vo.getUpdateUser());
			awardBudget.setCreateUserName(vo.getUpdateUser());
			awardBudget.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudget.setUpdateUser(vo.getUpdateUser());
			awardBudget.setUpdateUserName(vo.getUpdateUser());
			awardBudget.setEndDate(award.getFinalExpirationDate());
		} else {
			awardBudget.setVersionNumber(awardBudgetHeader.getVersionNumber());
			awardBudget.setSequenceNumber(awardBudgetHeader.getSequenceNumber());
			awardBudget.setBudgetTypeCode(awardBudgetHeader.getBudgetTypeCode());
			awardBudget.setBudgetType(awardBudgetHeader.getBudgetType());
			awardBudget.setCreateTimeStamp(awardBudgetHeader.getCreateTimeStamp());
			awardBudget.setCreateUser(awardBudgetHeader.getCreateUser());
			awardBudget.setCreateUserName(awardBudgetHeader.getCreateUserName());
			awardBudget.setUpdateTimeStamp(awardBudgetHeader.getUpdateTimeStamp());
			awardBudget.setUpdateUser(awardBudgetHeader.getUpdateUser());
			awardBudget.setUpdateUserName(awardBudgetHeader.getUpdateUserName());
			awardBudget.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(Constants.AWARD_BUDGET_STATUS_CODE_SUBMITTED));
			awardBudget.setBudgetStatusCode(Constants.AWARD_BUDGET_STATUS_CODE_SUBMITTED);
		}
		awardBudget.setObligatedTotal(awardBudgetService.calculateObligatedTotal(award.getAwardId(), award.getAwardNumber()));
		awardBudget.setAnticipatedTotal(awardBudgetService.calculateAnticipatedAmount(award.getAwardId(), award.getAwardNumber()));
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		awardBudget.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		awardBudget.setVirement(awardBudgetHeader.getVirement());
		awardBudget.setTotalCostShare(awardBudgetService.calculateCostShareIncludedInBudget(awardBudget.getAwardId()));
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao
				.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		awardBudget.setManpowerEnabled(manpowerEnabled);
		awardBudget.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
		awardBudget.setCumulativeVirement(awardBudgetHeader.getCumulativeVirement());
		awardBudgetHeader.setIsLatestVersion(false);
		awardBudget.setIsLatestVersion(true);
		awardBudgetDao.saveOrUpdateAwardBudgetOverView(awardBudgetHeader);
		awardBudgetDao.saveBudgetHeader(awardBudget);
		List<AwardBudgetPeriod> awardBudgetPeriod = new ArrayList<>();
		if (awardBudgetHeader.getBudgetPeriods() != null && !awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			List<AwardBudgetPeriod> budgetPeriods = copyAwardBudgetPeriods(award, awardBudgetHeader.getBudgetPeriods(),
					vo.getUpdateUser(), awardBudget, serviceRequestTypeCode);
			for (AwardBudgetPeriod periods : budgetPeriods) {
				periods.setBudgetId(awardBudget.getBudgetId());
				awardBudgetDao.saveOrUpdateAwardBudgetPeriod(periods);
				for (AwardBudgetDetail details : periods.getBudgetDetails()) {
					details.setBudgetId(awardBudget.getBudgetId());
					details.setBudgetPeriodId(periods.getBudgetPeriodId());
					awardBudgetDao.saveOrUpdateAwardBudgetLineItem(details);
				}
				awardBudgetPeriod.add(periods);
			}
			awardBudget.getBudgetPeriods().addAll(awardBudgetPeriod);
		} else {
			fetchAwardBudgetPeriods(awardBudget);
		}
		return awardBudget;
	}

	private List<AwardBudgetPeriod> copyAwardBudgetPeriods(Award copyAward, List<AwardBudgetPeriod> budgetPeriods, String updateUser, AwardBudgetHeader awardBudgetHeader, String serviceRequestTypeCode) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		boolean isSinglePeriodBudget = commonDao.getParameterValueAsBoolean(Constants.AWARD_BUDGET_SINGLE_PERIOD_ENABLED);
		for (AwardBudgetPeriod copiedAwardBudgetPeriod : budgetPeriods) {
			AwardBudgetPeriod awardBudgetPeriodData = new AwardBudgetPeriod();
			awardBudgetPeriodData.setBudgetId(awardBudgetHeader.getBudgetId());
//			if (copiedAwardBudgetPeriod.getVersionNumber() == null) {
//				awardBudgetPeriodData.setVersionNumber(1);
//			} else {
//				awardBudgetPeriodData.setVersionNumber(copiedAwardBudgetPeriod.getVersionNumber() + 1);
//			}
			awardBudgetPeriodData.setVersionNumber(copiedAwardBudgetPeriod.getVersionNumber());
			awardBudgetPeriodData.setAwardNumber(copyAward.getAwardNumber());
			awardBudgetPeriodData.setBudgetId(awardBudgetHeader.getBudgetId());
			if (isSinglePeriodBudget) {
				awardBudgetPeriodData.setBudgetPeriod(1);
			} else {
				awardBudgetPeriodData.setBudgetPeriod(copiedAwardBudgetPeriod.getBudgetPeriod());
			}
			awardBudgetPeriodData.setEndDate(copiedAwardBudgetPeriod.getEndDate());
			awardBudgetPeriodData.setStartDate(copiedAwardBudgetPeriod.getStartDate());
			awardBudgetPeriodData.setTotalCost(copiedAwardBudgetPeriod.getTotalCost());
			awardBudgetPeriodData.setTotalDirectCost(copiedAwardBudgetPeriod.getTotalDirectCost());
			awardBudgetPeriodData.setTotalIndirectCost(copiedAwardBudgetPeriod.getTotalIndirectCost());
			awardBudgetPeriodData.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetPeriodData.setUpdateUser(updateUser);
			awardBudgetPeriodData.setPeriodLabel(copiedAwardBudgetPeriod.getPeriodLabel());
			awardBudgetPeriodData.setSubcontractCost(copiedAwardBudgetPeriod.getSubcontractCost());
			awardBudgetPeriodData.setDevProposalId(copiedAwardBudgetPeriod.getDevProposalBudgetId());
			awardBudgetPeriodData.setDevProposalBudgetPeriod(copiedAwardBudgetPeriod.getDevProposalBudgetPeriod());
			awardBudgetPeriodData.setBudgetDetails(copyAwardBudgetDetails(copyAward, copiedAwardBudgetPeriod.getBudgetDetails(), updateUser, awardBudgetHeader, awardBudgetPeriodData, serviceRequestTypeCode));
			awardBudgetPeriods.add(awardBudgetPeriodData);
		}
		return awardBudgetPeriods;
	}

	private List<AwardBudgetDetail> copyAwardBudgetDetails(Award copyAward, List<AwardBudgetDetail> budgetDetails, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetPeriod awardBudgetPeriodData, String serviceRequestTypeCode) {
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		for (AwardBudgetDetail copiedAwardBudgetDetail : budgetDetails) {
			AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
			awardBudgetDetail.setBudgetPeriodId(awardBudgetPeriodData.getBudgetPeriodId());
			if (copiedAwardBudgetDetail.getVersionNumber() == null) {
				awardBudgetDetail.setVersionNumber(1);
			} else {
				awardBudgetDetail.setVersionNumber(copiedAwardBudgetDetail.getVersionNumber() + 1);
			}
			awardBudgetDetail.setAwardNumber(copyAward.getAwardNumber());
			awardBudgetDetail.setBudgetPeriod(copiedAwardBudgetDetail.getBudgetPeriod());
			awardBudgetDetail.setLineItemNumber(copiedAwardBudgetDetail.getLineItemNumber());
			awardBudgetDetail.setEndDate(copiedAwardBudgetDetail.getEndDate());
			awardBudgetDetail.setStartDate(copiedAwardBudgetDetail.getStartDate());
			awardBudgetDetail.setBudgetCategoryCode(copiedAwardBudgetDetail.getBudgetCategoryCode());
			awardBudgetDetail.setBudgetCategory(copiedAwardBudgetDetail.getBudgetCategory());
			awardBudgetDetail.setCostElementCode(copiedAwardBudgetDetail.getCostElementCode());
			awardBudgetDetail.setCostElement(copiedAwardBudgetDetail.getCostElement());
			awardBudgetDetail.setLineItemDescription(copiedAwardBudgetDetail.getLineItemDescription());
			awardBudgetDetail.setInternalOrderCode(copiedAwardBudgetDetail.getInternalOrderCode());
			if (!serviceRequestTypeCode.equals(Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE)) {
				awardBudgetDetail.setPrevLineItemCost(copiedAwardBudgetDetail.getPrevLineItemCost());
				awardBudgetDetail.setLineItemCost(copiedAwardBudgetDetail.getLineItemCost());
			} else {
				awardBudgetDetail.setPrevLineItemCost(copiedAwardBudgetDetail.getLineItemCost());
				awardBudgetDetail.setLineItemCost(copiedAwardBudgetDetail.getLineItemCost());
			}
			awardBudgetService.calculateBalanceToDateValue(awardBudgetDetail, awardBudgetHeader.getFundCode());
			awardBudgetDetail.setBudgetJustification(copiedAwardBudgetDetail.getBudgetJustification());
			awardBudgetDetail.setIsSystemGeneratedCostElement(copiedAwardBudgetDetail.getIsSystemGeneratedCostElement());
			awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetDetail.setUpdateUser(updateUser);
			awardBudgetDetail.setOnOffCampusFlag(copiedAwardBudgetDetail.getOnOffCampusFlag());
			awardBudgetDetail.setCostSharingAmount(copiedAwardBudgetDetail.getCostSharingAmount());
			awardBudgetDetail.setCostSharingPercentage(copiedAwardBudgetDetail.getCostSharingPercentage());
			awardBudgetDetail.setSystemGeneratedCEType(copiedAwardBudgetDetail.getSystemGeneratedCEType());
			awardBudgetDetail.setTbnId(copiedAwardBudgetDetail.getTbnId());
			awardBudgetDetail.setTbnPerson(copiedAwardBudgetDetail.getTbnPerson());
			awardBudgetDetail.setIsApplyInflationRate(copiedAwardBudgetDetail.getIsApplyInflationRate());
			awardBudgetDetail.setQuantity(copiedAwardBudgetDetail.getQuantity());
			awardBudgetDetail.setBudgetId(awardBudgetHeader.getBudgetId());
			awardBudgetDetail.setBudgetDetailId(null);
			awardBudgetDetail.setBudgetDetailCalcAmounts(copyAwardBudgetDetailCalcAmount(copiedAwardBudgetDetail.getBudgetDetailCalcAmounts(), updateUser, awardBudgetHeader, awardBudgetDetail));
			awardBudgetDetail.setBudgetRateAndBases(copyAwardBudgetRateAndBase(copiedAwardBudgetDetail.getBudgetRateAndBases(), updateUser, awardBudgetHeader, awardBudgetDetail));
			awardBudgetDetail.setPersonsDetails((copyAwardBudgetPersonalDetails(copiedAwardBudgetDetail.getPersonsDetails(), updateUser, awardBudgetHeader.getBudgetId(), awardBudgetDetail)));
			awardBudgetDao.saveOrUpdateAwardBudgetLineItem(awardBudgetDetail);
			awardBudgetDetails.add(awardBudgetDetail);
		}
		return awardBudgetDetails;
	}

	private List<AwardBudgetPersonalDetail> copyAwardBudgetPersonalDetails(
			List<AwardBudgetPersonalDetail> personsDetails, String updateUser, Integer awardBudgetHeaderId,
			AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetPersonalDetail> awardBudgetPersonDetails = new ArrayList<>();
		for (AwardBudgetPersonalDetail awardBudgetPersonalDetail : personsDetails) {
			AwardBudgetPersonalDetail awardBudgetPersonDetail = new AwardBudgetPersonalDetail();
			awardBudgetPersonDetail.setBudgetDetail(awardBudgetDetail);
			AwardBudgetPerson budgetperson = checkAwardBudgetPersonIsExist(awardBudgetHeaderId,
					awardBudgetPersonalDetail.getBudgetPerson(), updateUser);
			if (budgetperson != null) {
				awardBudgetPersonDetail.setBudgetPersonId(budgetperson.getBudgetPersonId());
				awardBudgetPersonDetail.setBudgetPerson(budgetperson);
			}
			awardBudgetPersonDetail.setUnderRecoveryAmount(awardBudgetPersonalDetail.getUnderRecoveryAmount());
			awardBudgetPersonDetail.setPercentageCharged(awardBudgetPersonalDetail.getPercentageCharged());
			awardBudgetPersonDetail.setPercentageEffort(awardBudgetPersonalDetail.getPercentageEffort());
			awardBudgetPersonDetail.setCostSharingAmount(awardBudgetPersonalDetail.getCostSharingAmount());
			awardBudgetPersonDetail.setCostSharingPercentage(awardBudgetPersonalDetail.getCostSharingPercentage());
			awardBudgetPersonDetail.setSalaryRequested(awardBudgetPersonalDetail.getSalaryRequested());
			awardBudgetPersonDetail.setTotalSalary(awardBudgetPersonalDetail.getTotalSalary());
			awardBudgetPersonDetail.setNoOfMonths(awardBudgetPersonalDetail.getNoOfMonths());
			awardBudgetPersonDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetPersonDetail.setUpdateUser(updateUser);
			awardBudgetPersonDetail.setStartDate(awardBudgetPersonalDetail.getStartDate());
			awardBudgetPersonDetail.setEndDate(awardBudgetPersonalDetail.getEndDate());
			awardBudgetPersonDetail.setInternalOrderCode(awardBudgetPersonalDetail.getInternalOrderCode());
			awardBudgetPersonDetails.add(awardBudgetPersonDetail);
		}
		return awardBudgetPersonDetails;
	}

	private AwardBudgetPerson checkAwardBudgetPersonIsExist(Integer awardBudgetHeaderId,
			AwardBudgetPerson sourceAwardBudgetPerson, String updateUser) {
		List<AwardBudgetPerson> awardBudgetPersons = awardBudgetDao.getBudgetPersons(awardBudgetHeaderId);
		boolean isAwardBudgetPersonExist = true;
		if (awardBudgetPersons == null || awardBudgetPersons.isEmpty()) {
			return prepareAwardBudgetPersons(awardBudgetHeaderId, updateUser, sourceAwardBudgetPerson);
		} else {
			for (AwardBudgetPerson awardBudgetPerson : awardBudgetPersons) {
				String tbnId = awardBudgetPerson.getTbnId();
				String sourceTBNId = sourceAwardBudgetPerson.getTbnId();
				String jobCode = awardBudgetPerson.getJobCode();
				String sourceJobCode = sourceAwardBudgetPerson.getJobCode();
				String appointmentTypeCode = awardBudgetPerson.getAppointmentTypeCode();
				String sourceAppointmentTypeCode = sourceAwardBudgetPerson.getAppointmentTypeCode();
				String personId = awardBudgetPerson.getPersonId();
				String sourcePersonId = sourceAwardBudgetPerson.getPersonId();
				Integer rolodexId = awardBudgetPerson.getRolodexId();
				Integer sourceRolodexId = sourceAwardBudgetPerson.getRolodexId();
				if (awardBudgetPerson.getPersonType().equals(Constants.TBN_PERSON_TYPE)) {
					if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))) {
						return awardBudgetPerson;
					} else if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if ((jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if (tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId)) {
						return awardBudgetPerson;
					} else if (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode)) {
						return awardBudgetPerson;
					} else if (appointmentTypeCode != null && sourceAppointmentTypeCode != null
							&& appointmentTypeCode.equals(sourceAppointmentTypeCode)) {
						return awardBudgetPerson;
					} else {
						isAwardBudgetPersonExist = false;
					}
				} else if (awardBudgetPerson.getPersonType().equals(Constants.EMPLOYEE_PERSON_TYPE)) {
					if ((personId != null && sourcePersonId != null && personId.equals(sourcePersonId))) {
						return awardBudgetPerson;
					} else {
						isAwardBudgetPersonExist = false;
					}
				} else if (awardBudgetPerson.getPersonType().equals(Constants.NON_EMPLOYEE_TYPE)) {
					if ((rolodexId != null && sourceRolodexId != null && rolodexId.equals(sourceRolodexId))) {
						return awardBudgetPerson;
					} else {
						isAwardBudgetPersonExist = false;
					}
				} else {
					if ((personId != null && sourcePersonId != null && personId.equals(sourcePersonId))) {
						return awardBudgetPerson;
					} else if ((rolodexId != null && sourceRolodexId != null && rolodexId.equals(sourceRolodexId))) {
						return awardBudgetPerson;
					} else if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))) {
						return awardBudgetPerson;
					} else if ((tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if ((jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode))
							&& (appointmentTypeCode != null && sourceAppointmentTypeCode != null
									&& appointmentTypeCode.equals(sourceAppointmentTypeCode))) {
						return awardBudgetPerson;
					} else if (tbnId != null && sourceTBNId != null && tbnId.equals(sourceTBNId)) {
						return awardBudgetPerson;
					} else if (jobCode != null && sourceJobCode != null && jobCode.equals(sourceJobCode)) {
						return awardBudgetPerson;
					} else if (appointmentTypeCode != null && sourceAppointmentTypeCode != null
							&& appointmentTypeCode.equals(sourceAppointmentTypeCode)) {
						return awardBudgetPerson;
					} else {
						isAwardBudgetPersonExist = false;
					}
				}
			}
			if (!isAwardBudgetPersonExist) {
				return prepareAwardBudgetPersons(awardBudgetHeaderId, updateUser, sourceAwardBudgetPerson);
			}
		}
		return null;
	}

	private AwardBudgetPerson prepareAwardBudgetPersons(Integer awardBudgetHeaderId, String updateUser,
			AwardBudgetPerson awardBudgetPerson) {
		AwardBudgetPerson budgetperson = new AwardBudgetPerson();
		budgetperson.setAppointmentType(awardBudgetPerson.getAppointmentType());
		budgetperson.setAppointmentTypeCode(awardBudgetPerson.getAppointmentTypeCode());
		budgetperson.setBudgetHeaderId(awardBudgetHeaderId);
		budgetperson.setCalculationBase(awardBudgetPerson.getCalculationBase());
		budgetperson.setDurationCost(awardBudgetPerson.getDurationCost());
		budgetperson.setEffectiveDate(awardBudgetPerson.getEffectiveDate());
		budgetperson.setJobCode(awardBudgetPerson.getJobCode());
		budgetperson.setJobCodes(awardBudgetPerson.getJobCodes());
		budgetperson.setPersonId(awardBudgetPerson.getPersonId());
		budgetperson.setPersonName(awardBudgetPerson.getPersonName());
		budgetperson.setPersonType(awardBudgetPerson.getPersonType());
		budgetperson.setRolodexId(awardBudgetPerson.getRolodexId());
		budgetperson.setSalaryAnniversaryDate(awardBudgetPerson.getSalaryAnniversaryDate());
		budgetperson.setTbnId(awardBudgetPerson.getTbnId());
		budgetperson.setTbnPerson(awardBudgetPerson.getTbnPerson());
		budgetperson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budgetperson.setUpdateUser(updateUser);
		awardBudgetDao.saveOrUpdateAwardBudgetPerson(budgetperson);
		return budgetperson;
	}

	private List<AwardBudgetDetailCalcAmount> copyAwardBudgetDetailCalcAmount(List<AwardBudgetDetailCalcAmount> budgetDetailCalcAmounts, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetDetailCalcAmount> newBudgetDetailCalcAmounts = new ArrayList<>();
		for (AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount : budgetDetailCalcAmounts) {
			AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmountData = new AwardBudgetDetailCalcAmount();
			awardBudgetDetailCalcAmountData.setBudgetDetailId(awardBudgetDetail.getBudgetDetailId());
			awardBudgetDetailCalcAmountData.setBudgetId(awardBudgetHeader.getBudgetId());
			awardBudgetDetailCalcAmountData.setBudgetPeriod(awardBudgetDetailCalcAmount.getBudgetPeriod());
			awardBudgetDetailCalcAmountData.setBudgetPeriodId(awardBudgetDetailCalcAmount.getBudgetPeriodId());
			awardBudgetDetailCalcAmountData.setLineItemNumber(awardBudgetDetailCalcAmount.getLineItemNumber());
			awardBudgetDetailCalcAmountData.setRateClassCode(awardBudgetDetailCalcAmount.getRateClassCode());
			awardBudgetDetailCalcAmountData.setRateTypeCode(awardBudgetDetailCalcAmount.getRateTypeCode());
			awardBudgetDetailCalcAmountData.setApplyRateFlag(awardBudgetDetailCalcAmount.getApplyRateFlag());
			awardBudgetDetailCalcAmountData.setCalculatedCost(awardBudgetDetailCalcAmount.getCalculatedCost());
			awardBudgetDetailCalcAmountData.setCalculatedCostSharing(awardBudgetDetailCalcAmount.getCalculatedCostSharing());
			awardBudgetDetailCalcAmountData.setRateTypeDescription(awardBudgetDetailCalcAmount.getRateTypeDescription());
			awardBudgetDetailCalcAmountData.setRateClass(awardBudgetDetailCalcAmount.getRateClass());
			awardBudgetDetailCalcAmountData.setRateType(awardBudgetDetailCalcAmount.getRateType());
			awardBudgetDetailCalcAmountData.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetDetailCalcAmountData.setUpdateUser(updateUser);
			awardBudgetDetailCalcAmountData.setApplicableRate(awardBudgetDetailCalcAmount.getApplicableRate());
			newBudgetDetailCalcAmounts.add(awardBudgetDetailCalcAmountData);
		}
		return newBudgetDetailCalcAmounts;
	}

	private List<AwardBudgetRateAndBase> copyAwardBudgetRateAndBase(List<AwardBudgetRateAndBase> budgetRateAndBases, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetRateAndBase> copiedBudgetRateAndBases = new ArrayList<>();
		for (AwardBudgetRateAndBase awardBudgetRateAndBase : budgetRateAndBases) {
			AwardBudgetRateAndBase awardBudgetRateAndBaseData = new AwardBudgetRateAndBase();
			awardBudgetRateAndBaseData.setBaseCost(awardBudgetRateAndBase.getBaseCost());
			awardBudgetRateAndBaseData.setBudgetDetail(awardBudgetDetail);
			awardBudgetRateAndBaseData.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetRateAndBaseData.setUpdateUser(updateUser);
			awardBudgetRateAndBaseData.setBudgetId(awardBudgetHeader.getBudgetId());
			awardBudgetRateAndBaseData.setBudgetPeriod(awardBudgetRateAndBase.getBudgetPeriod());
			awardBudgetRateAndBaseData.setBudgetPeriodId(awardBudgetRateAndBase.getBudgetPeriodId());
			awardBudgetRateAndBaseData.setLineItemNumber(awardBudgetRateAndBase.getLineItemNumber());
			awardBudgetRateAndBaseData.setRateClassCode(awardBudgetRateAndBase.getRateClassCode());
			awardBudgetRateAndBaseData.setRateTypeCode(awardBudgetRateAndBase.getRateTypeCode());
			copiedBudgetRateAndBases.add(awardBudgetRateAndBaseData);
		}
		return copiedBudgetRateAndBases;
	}

	private Award copyAwardBudgetDetails(Award orginalAward, Award copyAward, IntegrationVO vo) {
		copyAwardBudgetHeader(orginalAward, copyAward, vo);
		copyAward = awardDao.saveOrUpdateAwardDetails(copyAward);
		return copyAward;
	}

	private void copyCustomDatas(Integer awardId, List<CustomData> originalCustomDatas, String updateUser) {
		for (CustomData copiedCustomData : originalCustomDatas) {
			CustomData customData = new CustomData();
			customData.setCustomDataElementsId(copiedCustomData.getCustomDataElementsId());
			customData.setModuleItemCode(copiedCustomData.getModuleItemCode());
			customData.setModuleItemKey(awardId.toString());
			customData.setModuleSubItemCode(copiedCustomData.getModuleSubItemCode());
			customData.setValue(copiedCustomData.getValue());
			customData.setColumnId(copiedCustomData.getColumnId());
			customData.setModuleSubItemKey(copiedCustomData.getModuleSubItemKey());
			customData.setVersionNumber(copiedCustomData.getVersionNumber());
			customData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			customData.setUpdateUser(updateUser);
			customDataElementDao.saveOrUpdateCustomResponse(customData);
		}
	}

	private void copyQuestionnaireDatas(Integer orginalAwardId, IntegrationVO vo, String newAwardId) {
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setActionPersonId(vo.getPersonId());
		questionnaireDataBus.setActionUserId(vo.getUpdateUser());
		questionnaireDataBus.setModuleItemCode(Constants.AWARD_MODULE_CODE);
		questionnaireDataBus.setModuleItemKey(orginalAwardId.toString());
		questionnaireDataBus.setModuleSubItemCode(0);
		questionnaireDataBus.setModuleSubItemKey("0");
		questionnaireDataBus = questionnaireService.getApplicableQuestionnaire(questionnaireDataBus);
		List<HashMap<String, Object>> applicableQuestionnaire = questionnaireDataBus.getApplicableQuestionnaire();
		for (HashMap<String, Object> questionnaire : applicableQuestionnaire) {
			Integer questionnaireId = (questionnaire.get("QUESTIONNAIRE_ID") == null ? 0 : Integer.parseInt((questionnaire.get("QUESTIONNAIRE_ID").toString())));
			if (questionnaire.get("QUESTIONNAIRE_ANS_HEADER_ID") != null) {
				Integer questionnaireAnswerHeaderId = Integer.parseInt((questionnaire.get("QUESTIONNAIRE_ANS_HEADER_ID").toString()));
				String questionnaireCompleteFlag = (questionnaire.get("QUESTIONNAIRE_COMPLETED_FLAG") == null ? "N" : (questionnaire.get("QUESTIONNAIRE_COMPLETED_FLAG").toString()));
				questionnaireDataBus.setQuestionnaireId(questionnaireId);
				questionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireAnswerHeaderId);
				questionnaireDataBus.setModuleItemKey(newAwardId);
				questionnaireDataBus.setIsInserted(true);
				questionnaireDataBus.setQuestionnaireCompleteFlag(questionnaireCompleteFlag);
				questionnaireService.copyQuestionnaireAnswers(questionnaireDataBus);
			}
		}
	}

	private void copyAwardKPIs(Award award, List<AwardKPI> awardKPIs, String updateUser) {
		for (AwardKPI newAwardKPI : awardKPIs) {
			List<AwardKPICriteria> awardKPICriterias = new ArrayList<>();
			AwardKPI awardKPI = new AwardKPI();
			for (AwardKPICriteria newAwardKPICriteria : newAwardKPI.getAwardKPICriterias()) {
				AwardKPICriteria awardKPICriteria = new AwardKPICriteria();
				awardKPICriteria.setAwardKPI(awardKPI);
				awardKPICriteria.setKpiCriteriaType(newAwardKPICriteria.getKpiCriteriaType());
				awardKPICriteria.setKpiCriteriaTypeCode(newAwardKPICriteria.getKpiCriteriaTypeCode());
				awardKPICriteria.setTarget(newAwardKPICriteria.getTarget());
				awardKPICriteria.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardKPICriteria.setUpdateUser(updateUser);
				awardKPICriterias.add(awardKPICriteria);
			}
			awardKPI.setKpiType(newAwardKPI.getKpiType());
			awardKPI.setAwardId(award.getAwardId());
			awardKPI.setKpiTypeCode(newAwardKPI.getKpiTypeCode());
			awardKPI.setAwardNumber(award.getAwardNumber());
			awardKPI.setSequenceNumber(award.getSequenceNumber());
			awardKPI.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardKPI.setUpdateUser(updateUser);
			awardKPI.setAwardKPICriterias(awardKPICriterias);
			awardDao.saveOrUpdateAwardKPI(awardKPI);
		}
	}

	private void copyAwardMileStones(Award award, List<AwardMileStone> originalAwardMileStones, String updateUser) {
		List<AwardMileStone> copyMileStones = new ArrayList<>();
		for (AwardMileStone originalAwardMileStone : originalAwardMileStones) {
			AwardMileStone copyAwardMileStone = new AwardMileStone();
			copyAwardMileStone.setDuration(originalAwardMileStone.getDuration());
			copyAwardMileStone.setMilestone(originalAwardMileStone.getMilestone());
			copyAwardMileStone.setAwardId(award.getAwardId());
			copyAwardMileStone.setStartDate(originalAwardMileStone.getStartDate());
			copyAwardMileStone.setEndDate(originalAwardMileStone.getEndDate());
			copyAwardMileStone.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			copyAwardMileStone.setUpdateUser(updateUser);
			copyAwardMileStone.setSequenceNumber(award.getSequenceNumber());
			copyAwardMileStone.setAwardNumber(award.getAwardNumber());
			copyMileStones.add(awardDao.saveOrUpdateAwardMileStone(copyAwardMileStone));
		}
	}

	private void copyAwardBudgetHeader(Award orginalAward, Award copyAward, IntegrationVO vo) {
		List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(orginalAward.getAwardId());
		if (awardBudgetHeaders != null && !awardBudgetHeaders.isEmpty()) {
			for (AwardBudgetHeader awardBudgetHeader : awardBudgetHeaders) {
				fetchAwardBudgetPeriods(awardBudgetHeader);
				AwardBudgetHeader awardBudgetHeaderData = createAwardBudgetHeader(vo, copyAward, awardBudgetHeader, Constants.OTHER_SERVICE_REQUEST_TYPE_CODE);
				awardBudgetHeaderData.setFundCode(orginalAward.getAccountNumber());
				awardBudgetHeaderData.setFundCenter(orginalAward.getFundCenter());
				awardBudgetDao.saveBudgetHeader(awardBudgetHeaderData);
			}
			if (vo.getServiceRequestTypeCode()!= null &&  vo.getServiceRequestTypeCode().equals("2")) {
				AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(orginalAward.getAwardId());
				AwardBudgetHeader awardBudgetHeaderData = createAwardBudgetHeader(vo, copyAward, awardBudgetHeader, Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE);
				awardBudgetHeaderData.setFundCode(orginalAward.getAccountNumber());
				awardBudgetHeaderData.setFundCenter(orginalAward.getFundCenter());
				awardBudgetDao.saveBudgetHeader(awardBudgetHeaderData);
			}
		}
	}

	public List<AwardBudgetPeriod> fetchAwardBudgetPeriods(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetPeriods.addAll(awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId()));
		for (AwardBudgetPeriod period : awardBudgetPeriods) {
			period.getBudgetDetails().clear();
			List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
			budgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(period.getBudgetPeriodId());
			for (AwardBudgetDetail details : budgetDetails) {
				details.setBudgetDetailCalcAmounts(awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(details.getBudgetDetailId()));
				period.getBudgetDetails().add(details);
			}
			awardBudgetHeader.getBudgetPeriods().add(period);
		}
		
		return awardBudgetHeader.getBudgetPeriods();
	}

	public List<TaskAttachment> copyTaskAttachments(Integer taskId,Task task, String updateUser) {
		List<TaskAttachment> taskAttachments = integrationDao.getAllTaskAttachments(taskId);
		List<TaskAttachment> newTaskAttachments  = new ArrayList<>();
		for (TaskAttachment originalTaskAttachment : taskAttachments) {
			TaskAttachment taskAttachment = new TaskAttachment();
			taskAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			taskAttachment.setUpdateUser(updateUser);
			taskAttachment.setMimeType(originalTaskAttachment.getMimeType());
			taskAttachment.setFileName(originalTaskAttachment.getFileName());
			taskAttachment.setTask(task);
			taskAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(updateUser));
			TaskFileData fileData = new TaskFileData();
			TaskFileData taskFileData = taskDao.getFileDataById(originalTaskAttachment.getFileDataId());
			fileData.setData(taskFileData.getData());
			fileData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			fileData.setUpdateUser(updateUser);
			fileData = taskDao.saveFileData(fileData);
			taskAttachment.setFileDataId(fileData.getFileDataId());
			newTaskAttachments.add(taskAttachment);
		}
		return newTaskAttachments;
	}

	public String copyProposal(IntegrationVO vo) {
		Proposal originalProposal = proposalDao.fetchProposalById(vo.getProposalId());
		ProposalExtension originalProposalExtension = proposalDao.fetchProposalExtensionById(vo.getProposalId());
		Proposal copyProposal = new Proposal();
		ProposalExtension copyProposalExtension = new ProposalExtension();
		copyProposal = copyProposalMandatoryFields(vo, copyProposal, originalProposal);
		ProposalVO proposalVO = new ProposalVO();
		proposalVO.setUpdateUser(vo.getUpdateUser());
		proposalCopyService.copyProposalNonMandatoryFields(proposalVO, copyProposal, originalProposal, copyProposalExtension, originalProposalExtension);
		copyProposal.setStatusCode(Constants.COMPLETED);
		copyProposal.setProposalStatus(proposalLookUpDao.fetchProposalStatusByStatusCode(Constants.COMPLETED));
		proposalDao.saveOrUpdateProposal(copyProposal);
		List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(copyProposal.getProposalId());
		if(budgetHeaders != null && !budgetHeaders.isEmpty()) {
			BudgetHeader budgetHeader = budgetHeaders.get(0);
			budgetHeader.setIsFinalBudget(true);
			budgetDao.saveOrUpdateBudget(budgetHeader);
		}
		proposalVO.setProposal(copyProposal);
//		copyQuestionnaireDatasByModule(originalProposal.getProposalId().toString(), vo, copyProposal.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE);
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_EVALUATION_PANEL)) {
			proposalService.fetchAndSaveProposalEvaluationPanels(proposalVO);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private Proposal copyProposalMandatoryFields(IntegrationVO vo, Proposal copyProposal, Proposal originalProposal) {
		copyProposal.setTitle(vo.getTitle()+ "-" + commonDao.getUnitName(vo.getUnitNumber()) + "-" + personDao.getUserFullNameByUserName(vo.getPersonName()));
		copyProposal.setActivityTypeCode(originalProposal.getActivityTypeCode());
		copyProposal.setActivityType(originalProposal.getActivityType());
		copyProposal.setTypeCode(originalProposal.getTypeCode());
		copyProposal.setProposalType(originalProposal.getProposalType());
		copyProposal.setHomeUnitNumber(vo.getUnitNumber());
		copyProposal.setHomeUnitName(commonDao.getUnitName(copyProposal.getHomeUnitNumber()));
		copyProposal.setSponsorCode(originalProposal.getSponsorCode());
		copyProposal.setSponsor(commonDao.getSponsorById(originalProposal.getSponsorCode()));
		copyProposal.setStartDate(originalProposal.getStartDate());
		copyProposal.setEndDate(originalProposal.getEndDate());
		copyProposal.setSubmissionDate(originalProposal.getSubmissionDate());
		copyProposal.setInternalDeadLineDate(originalProposal.getInternalDeadLineDate());
		copyProposal.setGrantCallClosingDate(originalProposal.getGrantCallClosingDate());
		copyProposal.setGrantCallName(originalProposal.getGrantCallName());
		copyProposal = proposalDao.saveOrUpdateProposal(copyProposal);
		List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(originalProposal.getProposalId());
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			copyProposalPersons(copyProposal.getProposalId(), vo.getUpdateUser(), proposalPersons, vo);
		}
		copyProposal = proposalDao.saveOrUpdateProposal(copyProposal);
		List<ProposalPersonRoles> proposalPersonRoles = proposalLookUpDao.fetchProposalPersonRoles(originalProposal.getProposalId(), null);
		if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
			copyProposalPersonRoles(copyProposal.getProposalId(), vo.getUpdateUser(), proposalPersonRoles);
		}
		return copyProposal;
	}

	private List<ProposalPersonRoles> copyProposalPersonRoles(Integer proposalId, String updateUser, List<ProposalPersonRoles> proposalPersonRoles) {
		List<ProposalPersonRoles> newProposalPersonRoles = new ArrayList<>();
		String personId = personDao.getPersonIdByUserName(updateUser);
		boolean personAggregatorRoleExist = false;
		if (proposalPersonRoles != null && !proposalPersonRoles.isEmpty()) {
			personAggregatorRoleExist = checkProposalPersonAggregatorRoleExist(personId, proposalPersonRoles);
			for (ProposalPersonRoles copiedProposalPersonRole : proposalPersonRoles) {
				ProposalPersonRoles newProposalPersonRole = createProposalPersonRole(proposalId, updateUser,
						copiedProposalPersonRole, true, personId);
				newProposalPersonRoles.add(proposalDao.saveProposalPersonRole(newProposalPersonRole));
			}
		} else {
			ProposalPersonRoles newProposalPersonRole = createProposalPersonRole(proposalId, updateUser, null,
					personAggregatorRoleExist, personId);
			newProposalPersonRoles.add(proposalDao.saveProposalPersonRole(newProposalPersonRole));
			personAggregatorRoleExist = true;
		}
		if (!personAggregatorRoleExist) {
			ProposalPersonRoles newProposalPersonRole = createProposalPersonRole(proposalId, updateUser, null,
					personAggregatorRoleExist, personId);
			newProposalPersonRoles.add(proposalDao.saveProposalPersonRole(newProposalPersonRole));
		}
		return newProposalPersonRoles;
	}

	private boolean checkProposalPersonAggregatorRoleExist(String personId, List<ProposalPersonRoles> proposalPersonRoles) {
		for (ProposalPersonRoles copiedProposalPersonRole : proposalPersonRoles) {
			String copiedPersonId = copiedProposalPersonRole.getPersonId();
			if (copiedPersonId != null && copiedPersonId.equals(personId) && (copiedProposalPersonRole.getRoleId().equals(Constants.PROPOSAL_AGGREGATOR_ROLE_ID))) {
					return true;
			}
		}
		return false;
	}

	private ProposalPersonRoles createProposalPersonRole(Integer proposalId, String updateUser,
			ProposalPersonRoles proposalPersonRole, boolean personAggregatorRoleExist, String personId) {
		ProposalPersonRoles newProposalPersonRole = new ProposalPersonRoles();
		newProposalPersonRole.setProposalId(proposalId);
		if (!personAggregatorRoleExist) {
			newProposalPersonRole.setPersonId(personId);
			newProposalPersonRole.setPerson(personDao.getPersonDetailById(newProposalPersonRole.getPersonId()));
			newProposalPersonRole.setRoleId(Constants.PROPOSAL_AGGREGATOR_ROLE_ID);
		} else {
			newProposalPersonRole.setPersonId(proposalPersonRole.getPersonId());
			newProposalPersonRole.setPerson(proposalPersonRole.getPerson());
			newProposalPersonRole.setRoleId(proposalPersonRole.getRoleId());
		}
		newProposalPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		newProposalPersonRole.setUpdateUser(updateUser);
		return newProposalPersonRole;
	}

	private List<ProposalPerson> copyProposalPersons(Integer proposalId, String updateUser, List<ProposalPerson> proposalPersons, IntegrationVO vo) {
		List<ProposalPerson> newProposalPersons = new ArrayList<>();
		for (ProposalPerson copiedPersonDetail : proposalPersons) {
			ProposalPerson personDetail = new ProposalPerson();
			personDetail.setProposalId(proposalId);
			personDetail.setPercentageOfEffort(copiedPersonDetail.getPercentageOfEffort());
			if (copiedPersonDetail.getIsPi() == true) {
				Person person = personDao.getPersonDetailById(personDao.getPersonIdByUserName(vo.getPersonName()));
				personDetail.setPersonId(person.getPersonId());
				personDetail.setFullName(person.getFullName());
				personDetail.setEmailAddress(person.getEmailAddress());
			} else {
				personDetail.setPersonId(copiedPersonDetail.getPersonId());
				personDetail.setFullName(copiedPersonDetail.getFullName());
				personDetail.setEmailAddress(copiedPersonDetail.getEmailAddress());
			}
			personDetail.setProposalPersonRole(copiedPersonDetail.getProposalPersonRole());
			personDetail.setPersonRoleId(copiedPersonDetail.getPersonRoleId());
			personDetail.setIsPi(copiedPersonDetail.getIsPi());
			personDetail.setRolodexId(copiedPersonDetail.getRolodexId());
			personDetail.setUpdateUser(updateUser);
			personDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			personDetail.setIsMultiPi(copiedPersonDetail.getIsMultiPi());
			personDetail.setDesignation(copiedPersonDetail.getDesignation());
			List<ProposalPersonUnit> units = copiedPersonDetail.getUnits();
			if (units != null && !units.isEmpty()) {
				personDetail.getUnits().addAll(copyProposalPersonUnits(copiedPersonDetail, personDetail, updateUser, vo));
			}
			List<ProposalPersonAttachment> personAttachments = copiedPersonDetail.getProposalPersonAttachment();
			if (personAttachments != null && !personAttachments.isEmpty()) {
				personDetail.getProposalPersonAttachment().addAll(copyProposalPersonAttchment(copiedPersonDetail, personDetail, updateUser));
			}
			proposalModuleDao.saveOrUpdateProposalPerson(personDetail);
			newProposalPersons.add(personDetail);
		}
		return newProposalPersons;
	}

	private List<ProposalPersonAttachment> copyProposalPersonAttchment(ProposalPerson copiedPersonDetail, ProposalPerson personDetail, String updateUser) {
		List<ProposalPersonAttachment> proposalPersonAttachments = copiedPersonDetail.getProposalPersonAttachment();
		List<ProposalPersonAttachment> newproposalPersonAttachments = new ArrayList<>();
		for (ProposalPersonAttachment copiedProposalPersonAttachment : proposalPersonAttachments) {
			ProposalPersonAttachment attachment = new ProposalPersonAttachment();
			attachment.setProposalPerson(personDetail);
			attachment.setDescription(copiedProposalPersonAttachment.getDescription());
			attachment.setFileName(copiedProposalPersonAttachment.getFileName());
			attachment.setMimeType(copiedProposalPersonAttachment.getMimeType());
			FileData fileData = commonDao.getFileDataById(copiedProposalPersonAttachment.getFileDataId());
			FileData file = new FileData();
			file.setAttachment(fileData.getAttachment());
			file = commonDao.saveFileData(file);
			attachment.setFileDataId(file.getFileDataId());
			attachment.setUpdateUser(updateUser);
			attachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			newproposalPersonAttachments.add(attachment);
		}
		return newproposalPersonAttachments;
	}

	private List<ProposalPersonUnit> copyProposalPersonUnits(ProposalPerson copiedPersonDetail, ProposalPerson personDetail, String updateUser, IntegrationVO vo) {
		List<ProposalPersonUnit> proposalPersonUnits = copiedPersonDetail.getUnits();
		List<ProposalPersonUnit> newProposalPersonUnits = new ArrayList<>();
		for (ProposalPersonUnit copiedPersonPersonUnit : proposalPersonUnits) {
			ProposalPersonUnit personUnit = new ProposalPersonUnit();
			personUnit.setProposalPerson(personDetail);
			if (personDetail.getIsPi() == true) {
				personUnit.setUnitNumber(vo.getUnitNumber());
				personUnit.setLeadUnit(true);
				personUnit.setUnit(commonDao.getUnitByUnitNumber(vo.getUnitNumber()));
			} else {
				personUnit.setUnitNumber(copiedPersonPersonUnit.getUnitNumber());
				personUnit.setLeadUnit(copiedPersonPersonUnit.isLeadUnit());
				personUnit.setUnit(copiedPersonPersonUnit.getUnit());
			}
			personUnit.setUpdateUser(updateUser);
			personUnit.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			newProposalPersonUnits.add(personUnit);
		}
		return newProposalPersonUnits;
	}

	public void createAwardVariationRequest(IntegrationVO vo, Award copyAward) {
		AwardVO awardVO = new AwardVO();
		awardVO.setAwardId(copyAward.getAwardId());
		awardVO.setAwardNumber(copyAward.getAwardNumber());
		Person person = personDao.getPersonDetailById(personDao.getPersonIdByUserName(vo.getPersonName()));
		awardVO.setPersonId(person.getPersonId());
		awardVO.setUserName(person.getPrincipalName());
		awardVO.setUserFullName(person.getFullName());
		awardVO.setServiceRequestTypeCode(vo.getServiceRequestTypeCode());
		awardVO.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(vo.getServiceRequestTypeCode()));
		awardVO.setIsVariationRequest(true);
		awardVO.setSubject(awardVO.getServiceRequestType().getSubject());
		awardVO.setDescription(awardVO.getServiceRequestType().getInstruction());
		awardVO.setUpdateUser(person.getPrincipalName());
		awardVO.setAward(copyAward);
		awardVO.setIsFeededAwardId(true);
		String variationAwardId = createAwardVariationRequestForDataFeed(awardVO);
		submitAward(person.getPersonId(), variationAwardId, copyAward.getAwardNumber(), person.getPrincipalName());
	}

	public void submitAward(String personId, String awardId, String awardNumber, String userName){
		AwardVO awardVO = new AwardVO();
		awardVO.setAwardId(Integer.parseInt(awardId));
		awardVO.setAwardNumber(awardNumber);
		awardVO.setPersonId(personId);
		awardVO.setUserName(userName);
		awardWorkflowService.submitAward(awardVO);
	}

	@Override
	public String checkForTheSAPFeedResponse(String awardNumber, Integer awardId) {
		return integrationDao.checkForTheSAPFeedResponse(awardNumber, awardId);
	}

	public String createAwardVariationRequestForDataFeed(AwardVO vo) {
		if (vo.getServiceRequestId() == null) {
			vo.setServiceRequest(createVariationRequestForAward(vo));
			if (vo.getIsFeededAwardId().equals(Boolean.TRUE)) {
				return awardVersionService.copyAward(vo);
			} else {
				awardVersionService.copyAward(vo);
			}
		} else {
			vo.setServiceRequest(serviceRequestDao.fetchServiceRequestById(vo.getServiceRequestId()));
			vo.setAward(awardDao.getAwardDetailsById(Integer.parseInt(vo.getServiceRequest().getModuleItemKey())));
		}
		if (vo.getServiceRequestTypeCode() != null) {
			awardVersionService.addAwardEditableFields(vo);
		}

		if (vo.getAward().getLeadUnitNumber() != null && vo.getAward().getAwardId() != null
				&& vo.getPersonId() != null) {
			vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AWARD_MODULE_CODE,
					vo.getPersonId(), vo.getAward().getLeadUnitNumber(), vo.getAward().getAwardId()));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private ServiceRequest createVariationRequestForAward(AwardVO vo) {
		ServiceRequest serviceRequest = new ServiceRequest();
		serviceRequest.setStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT);
		serviceRequest.setServiceRequestStatus(serviceRequestDao.fetchStatusByStatusCode(Constants.SERVICE_REQUEST_STATUS_CODE_DRAFT));
		serviceRequest.setModuleCode(Constants.AWARD_MODULE_CODE);
		if (Boolean.TRUE.equals(vo.getIsAwardModification())) {
			serviceRequest.setTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.ALL_SERVICE_REQUEST_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.ALL_SERVICE_REQUEST_TYPE_CODE);
		} else if (Boolean.TRUE.equals(vo.getIsVariationRequest())) {
			serviceRequest.setTypeCode(vo.getServiceRequestTypeCode());
			serviceRequest.setServiceRequestType(vo.getServiceRequestType());
		} else if (Boolean.TRUE.equals(vo.getIsProjectClosure())) {
			serviceRequest.setTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.PROJECT_CLOSURE_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.PROJECT_CLOSURE_TYPE_CODE);
		} else if (Boolean.TRUE.equals(vo.getIsAwardOutcome())) {
			serviceRequest.setTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
			serviceRequest.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(Constants.AWARD_OUTCOME_TYPE_CODE));
			vo.setServiceRequestTypeCode(Constants.AWARD_OUTCOME_TYPE_CODE);
		}
		serviceRequest.setModuleItemKey(vo.getAwardId().toString());
		serviceRequest.setSubject(vo.getSubject());
		serviceRequest.setDescription(vo.getDescription());
		serviceRequest.setReporterPersonId(vo.getPersonId());
		serviceRequest.setIsSystemGenerated(Boolean.TRUE);
		serviceRequest = serviceRequestDao.saveOrUpdateServiceRequest(serviceRequest);
		return serviceRequest;
	}

}


package com.polus.fibicomp.claims.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.Period;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.util.claim.JxlsAggregator;
import com.polus.fibicomp.util.claim.JxlsDateFormatter;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.jxls.common.Context;
import org.jxls.util.JxlsHelper;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.expense.dao.AwardExpenseDao;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.pojo.ClaimAttachment;
import com.polus.fibicomp.claims.pojo.ClaimFundingScheme;
import com.polus.fibicomp.claims.pojo.ClaimInvoice;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceDetails;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceMetadata;
import com.polus.fibicomp.claims.pojo.ClaimManpower;
import com.polus.fibicomp.claims.pojo.ClaimOutputGstTaxCode;
import com.polus.fibicomp.claims.pojo.ClaimStatus;
import com.polus.fibicomp.claims.pojo.ClaimSummary;
import com.polus.fibicomp.claims.pojo.ClaimSummaryDetails;
import com.polus.fibicomp.claims.vo.ClaimsVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.LetterTemplateType;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.util.claim.JxlsComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Service(value = "claimsService")
@Transactional
public class ClaimsServiceImpl implements ClaimsService {

	protected static Logger logger = LogManager.getLogger(ClaimsServiceImpl.class.getName());
	
	@Autowired
	@Qualifier(value = "claimsDao")
	private ClaimsDao claimsDao;
	
	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	public InboxService inboxService;

	@Autowired
	public InboxDao inboxDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	public AuthorizationService authorizationService;
	
	@Autowired
	private PrintService printService;
	
	@Autowired
	private ExcelityService excelityService;

	@Autowired
	private AwardExpenseDao awardExpenseDao;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private ManpowerDao manpowerDao;

	private static final String CLAIMVO = "claimVO";
	private static final String GROUPED_TRANSACTIONS = "groupedTransactions";
	private static final String DELETE_CLAIM = "DELETE_CLAIM";
	private static final String CLAIM_PREPARER = "CLAIM_PREPARER";

	@Override
	public String saveOrUpdateClaim(ClaimsVO claimsVO) {
		Claim claim = claimsVO.getClaim();
		if(claim.getClaimId() == null) {
			claim = claimsDao.insertClaim(claim, claimsVO.getAcType());
			BigDecimal amountReq = claimsDao.getClaimSummaryAmountReq(claim.getClaimId(), "EOM");
			claimsDao.updateClaimActionLog(claim.getClaimId(), Constants.CLAIM_STATUS_CODE_PENDING, "I");
			claim = claimsDao.getClaim(claim.getClaimId());
			claimsVO.setClaim(claim);
			updateClaimSummaryAmounts(claim, amountReq);
			if(claimsVO.getClaim().getOverHeadPercentage() != null) {
				BigDecimal totalAmountReq = claimsDao.getTotalAmountRequestedForClaim(claim.getClaimId());
				BigDecimal adjAmountReq = totalAmountReq.multiply(claimsVO.getClaim().getOverHeadPercentage().divide(new BigDecimal(100)));
				claim.setAdjustedIndirectCost(adjAmountReq);
			}
			claimsVO.setClaim(claim);
			claimsVO.getClaim().setHostUnitDescription(commonDao.getUnitByUnitNumber(claimsVO.getClaim().getHostUnitNumber()).getUnitName());
			claimsDao.saveOrUpdateClaim(claim);
			return commonDao.convertObjectToJSON(claimsVO);
		}else {
			Claim lastUpdateClaim = claimsDao.getClaim(claim.getClaimId());
			if(lastUpdateClaim.getStartDate().compareTo(claim.getStartDate()) != 0 || lastUpdateClaim.getEndDate().compareTo(claim.getEndDate()) != 0) {
				claim.setAward(lastUpdateClaim.getAward());
				claimsDao.updateClaimSummaryTransactions(claim);
				BigDecimal amountReq =  claimsDao.getClaimSummaryAmountReq(claim.getClaimId(), "EOM");
				if(claimsVO.getClaim().getOverHeadPercentage() != null) {
					BigDecimal totalAmountReq = claimsDao.getTotalAmountRequestedForClaim(claim.getClaimId()).add(amountReq == null ? BigDecimal.ZERO : amountReq);
					BigDecimal adjAmountReq = totalAmountReq.multiply(claimsVO.getClaim().getOverHeadPercentage().divide(new BigDecimal(100)));
					claim.setAdjustedIndirectCost(adjAmountReq);
					lastUpdateClaim.setAdjustedIndirectCost(adjAmountReq);
				}
				updateClaimSummaryAmounts(claim, amountReq);
			}
			claim.setIdcAmountForcasted(lastUpdateClaim.getIdcAmountForcasted());
			claim.setAdjustedIndirectCost(lastUpdateClaim.getAdjustedIndirectCost());
			commonDao.detachEntityFromSession(lastUpdateClaim);
			claim.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			claimsDao.saveOrUpdateClaim(claim);
			claim.setUpdateUserName(AuthenticatedUser.getLoginUserFullName());
			claimsVO.setClaim(claim);
			return commonDao.convertObjectToJSON(claimsVO);	
		}
	}

	private void updateClaimSummaryAmounts(Claim claim, BigDecimal amountReq) {
		List<ClaimSummary> claimSummary = claimsDao.loadClaimSummary(claim.getClaimId());
		BigDecimal amountReqRSS = claimsDao.getClaimSummaryAmountReq(claim.getClaimId(), "RSS");
		BigDecimal totalCommitmentsUptoClaim = BigDecimal.ZERO;
		BigDecimal totalAmountForecasted = BigDecimal.ZERO;
		for (ClaimSummary summary : claimSummary) {
			if (summary.getBudgetCategoryCode().equals("EOM")) {
				summary.setAmountRequested(amountReq);
			} else if(summary.getBudgetCategoryCode().equals("RSS")) {
				summary.setAmountRequested(amountReqRSS);
			}			
			BigDecimal cumAmountUntilLastClaim = BigDecimal.ZERO;
			BigDecimal totalExpIncuredUptoClaim = BigDecimal.ZERO;
			BigDecimal fundBalanceAtEndOfClaim = BigDecimal.ZERO;
			BigDecimal amountReqForCurrentClaim = BigDecimal.ZERO;
			BigDecimal cumExpenseUptoPrevClaimPeriod = BigDecimal.ZERO;
			ClaimSummary previousClaimSummary = claimsDao.getPrevClaimSummaryOfBudgetCategory(claim.getClaimId(), claim.getAwardNumber(), summary.getBudgetCategoryCode());
			if (previousClaimSummary != null) {
				BigDecimal amountRequiredInPrevClaim = previousClaimSummary.getAmountReqForCurrentClaim() == null ? BigDecimal.ZERO : previousClaimSummary.getAmountReqForCurrentClaim();
				ClaimFundingScheme fundingScheme = claimsDao.getClaimFundingScheme(claim.getAwardId());
				if(fundingScheme != null && fundingScheme.getOverrideNegativeAmount() != null && fundingScheme.getOverrideNegativeAmount().equals(Boolean.TRUE))
					amountRequiredInPrevClaim = new BigDecimal(Math.max(Double.valueOf(amountRequiredInPrevClaim.toString()), 0));
				cumAmountUntilLastClaim = (previousClaimSummary.getCumClaimAmountUptoClaim() == null ? BigDecimal.ZERO: previousClaimSummary.getCumClaimAmountUptoClaim()).add(amountRequiredInPrevClaim);
				cumExpenseUptoPrevClaimPeriod = previousClaimSummary.getCumExpenseUptoPrevClaim() == null ? BigDecimal.ZERO: previousClaimSummary.getCumExpenseUptoPrevClaim()
						.add(previousClaimSummary.getAmountRequested() == null ? BigDecimal.ZERO : previousClaimSummary.getAmountRequested());												   
			}
			BigDecimal commitmentsAmtUptoPeriod = BigDecimal.ZERO;
			totalExpIncuredUptoClaim = cumExpenseUptoPrevClaimPeriod.add(summary.getAmountRequested() == null ? BigDecimal.ZERO : summary.getAmountRequested());
			fundBalanceAtEndOfClaim = cumAmountUntilLastClaim.subtract(totalExpIncuredUptoClaim);
			amountReqForCurrentClaim = commitmentsAmtUptoPeriod.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted()).subtract(fundBalanceAtEndOfClaim);
			summary.setCumClaimAmountUptoClaim(cumAmountUntilLastClaim);
			summary.setCumExpenseUptoPrevClaim(cumExpenseUptoPrevClaimPeriod);
			summary.setCommitmentsUptoPrevClaim(commitmentsAmtUptoPeriod);
			summary.setAmountReqForCurrentClaim(amountReqForCurrentClaim);
			claimsDao.saveOrUpdateClaimSummary(summary);
			totalCommitmentsUptoClaim = totalCommitmentsUptoClaim.add(summary.getCommitmentsUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCommitmentsUptoPrevClaim());
			totalAmountForecasted = totalAmountForecasted.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted());
		}
		BigDecimal overheadPercentage = claim.getOverHeadPercentage() == null ? BigDecimal.ZERO : claim.getOverHeadPercentage();
		claim.setIdcAmountForcasted(totalAmountForecasted.multiply(overheadPercentage.divide(new BigDecimal(100))));
		claim.setIdcCommitmentUptoPrevClaim(totalCommitmentsUptoClaim.multiply(overheadPercentage.divide(new BigDecimal(100))));	
		claim.setIdcCumClaimAmtUptoClaim(getIndirectCostCumClaimAmountUptoPrevClaim(claim.getClaimId(), claim.getAwardNumber()));
	}

	private BigDecimal getIndirectCostCumClaimAmountUptoPrevClaim(Integer claimId, String awardNumber) {
		Integer prevClaimId = claimsDao.getPrevClaimId(claimId, awardNumber);
		BigDecimal cumClaimAmountUptoPrevClaim = BigDecimal.ZERO;
		if(prevClaimId != null) {
			BigDecimal totalAmountReqForNextPeriod = claimsDao.totalAmountReqForNextPeriod(prevClaimId);
			BigDecimal idcCumClaimAmtUptoClaim = claimsDao.getIdcCumClaimAmtUptoClaim(prevClaimId);
			cumClaimAmountUptoPrevClaim = idcCumClaimAmtUptoClaim == null ? totalAmountReqForNextPeriod : 
				idcCumClaimAmtUptoClaim.add(totalAmountReqForNextPeriod);
		}
		return cumClaimAmountUptoPrevClaim;
	}
	
	@Override
	public String loadClaimEndorsment(ClaimsVO claimsVO) {
		Claim claim = claimsDao.getClaim(claimsVO.getClaimId());
		claimsVO.setClaim(claim);
		loadClaimDetails(claimsVO);
		loadClaimWorkflowDetails(claimsVO);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public void loadClaimDetails(ClaimsVO claimsVO) {
		Claim claim = claimsVO.getClaim();
		Boolean enableClaimStartEndDate = Boolean.FALSE;
		List<String> claimStatuses = new ArrayList<>();
		claimStatuses.add(Constants.CLAIM_STATUS_CODE_REVISION);
		claimStatuses.add(Constants.CLAIM_STATUS_CODE_REVISION_BY_FA);
		Boolean previousClaimEditable = claimsDao.checkIfPrevClaimIsInEditMode(claimsVO.getClaimId(), claim.getAwardNumber(), claimStatuses);
		if (claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_PENDING) || claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_REVISION) || claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_REVISION_BY_FA)) {
			enableClaimStartEndDate = claimsDao.checkIfLastClaimByParams(claimsVO.getClaimId(), claim.getAwardNumber());
		}
		claimsVO.setPreviousClaimEditable(previousClaimEditable);
		claimsVO.setEnableClaimStartEndDate(enableClaimStartEndDate);
		claimsVO.setLastClaimEndDate(claimsDao.getLastClaimEndDate(claimsVO.getClaim().getAwardNumber(), claimsVO.getClaim().getClaimId()));
		claimsVO.getClaim().setHostUnitDescription(commonDao.getUnitByUnitNumber(claimsVO.getClaim().getHostUnitNumber()).getUnitName());
		claimsVO.getClaim().setUpdateUserName(personDao.getUserFullNameByUserName(claimsVO.getClaim().getUpdateUser()));
		claimsVO.getClaim().setCreateUserName(personDao.getUserFullNameByUserName(claimsVO.getClaim().getCreateUser()));
	}

	@Override
	public String submitClaimDetails(ClaimsVO claimVO) {
		claimsDao.updateClaimDetailByParams(claimVO.getClaimId(), Constants.CLAIM_STATUS_CODE_APPROVAL_IN_PROGRESS, true, claimVO.getUpdateUser(), null);
		Claim claim = claimsDao.getClaim(claimVO.getClaimId());
		buildClaimWorkflow(claimVO);
		fetchPreviousWorkFlowsList(claimVO);
		if (claimVO.getWorkflow() != null && claimVO.getWorkflow().getWorkflowDetails().isEmpty() || claimVO.getWorkflow() == null) {
			claim.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_APPROVED));
			claimsDao.updateClaimDetailByParams(claimVO.getClaimId(), Constants.CLAIM_STATUS_CODE_APPROVED, true, claimVO.getUpdateUser(), null);
			String createUserId = personDao.getPersonIdByUserName(claim.getCreateUser());
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			commonService.setNotificationRecipients(createUserId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendClaimNotification(claimVO, Constants.CLAIM_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
		if (claimVO.getSubmitUserFullName() != null) {
			claim.setSubmitUserFullName(claimVO.getSubmitUserFullName());
		}
		claimVO.setClaim(claim);
		inboxDao.markReadMessage(Constants.CLAIM_MODULE_CODE, claimVO.getClaimId().toString(), null, Constants.MESSAGE_TYPE_CLAIM_REJECT, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
		businessRuleService.evaluateAndSentNotification(Constants.CLAIM_MODULE_CODE, Constants.CLAIM_SUBMODULE_CODE, claimVO.getClaimId().toString(), Constants.SUBMODULE_ITEM_KEY, claimVO.getPersonId(), claimVO.getUpdateUser(),getClaimPlaceholders(claimVO));
		loadClaimDetails(claimVO);
		return commonDao.convertObjectToJSON(claimVO);
	}

	private ClaimsVO buildClaimWorkflow(ClaimsVO claimVO) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		evaluateValidationRuleVO.setSubModuleCode(Constants.CLAIM_SUBMODULE_CODE);
		evaluateValidationRuleVO.setModuleItemKey(claimVO.getClaimId().toString());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		evaluateValidationRuleVO.setLogginPersonId(claimVO.getPersonId());
		evaluateValidationRuleVO.setUpdateUser(claimVO.getUpdateUser());
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			claimVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(claimVO.getClaimId().toString(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		claimVO.setCanApproveRouting(canApproveRouting.toString());
		claimVO.setIsFinalApprover(isFinalApprover);
		if (claimVO.getWorkflow() != null) {
			claimVO.setSubmitUserFullName(personDao.getPersonFullNameByPersonId(claimVO.getWorkflow().getWorkflowStartPerson()));
			List<WorkflowDetail> workflowDetails = claimVO.getWorkflow().getWorkflowDetails();
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
						claimVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						claimVO.setMapId(workflowDetail.getMapId());
					}
				}
			}
			sendClaimNotification(claimVO, Constants.CLAIM_SUBMITTED_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
		return claimVO;
	}

	@Override
	public ClaimsVO sendClaimNotification(ClaimsVO claimVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.CLAIM_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(claimVO.getClaimId().toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO.setPlaceHolder(getClaimPlaceholders(claimVO));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
		return claimVO;
	}

	private Map<String, String> getClaimPlaceholders(ClaimsVO claimVO) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{WORKFLOW_COMMENT}", claimVO.getApproveComment() != null ? claimVO.getApproveComment() : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(claimVO.getApproverStopNumber(),claimVO.getMapId(),claimVO.getWorkFlowDetailId()); 
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	private ClaimsVO fetchPreviousWorkFlowsList(ClaimsVO claimVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(claimVO.getClaimId().toString(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			claimVO.setWorkflowList(workFlows);
		}
		return claimVO;
	}

	public void loadClaimWorkflowDetails(ClaimsVO claimVO) {
		Claim claim = claimVO.getClaim();
		if (!claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_PENDING)
				|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)
						&& (!claim.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_PENDING)))) {
			canClaimTakeRoutingAction(claimVO);
			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(claimVO.getClaimId().toString(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
			if (workflow != null) {
				workflowService.prepareWorkflowDetails(workflow);
				claimVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(claimVO.getClaimId().toString(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					claimVO.setWorkflowList(workFlows);
				}
			}
		}
		Integer canApproveRouting = businessRuleDao.canApproveRouting(claimVO.getClaimId().toString(),claimVO.getPersonId(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
		claimVO.setCanApproveRouting(canApproveRouting.toString());
		claimVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(claimVO.getClaimId().toString(), claimVO.getPersonId(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE));
		claimVO.setIsSubmit("1");
		claimVO.setClaim(claim);
		claimVO.setAvailableRights(authorizationService.allDepartmentPermission(Constants.CLAIM_MODULE_CODE, claimVO.getPersonId(), claim.getAward().getLeadUnitNumber(), claimVO.getClaimId()));
	}

	@Override
	public void canClaimTakeRoutingAction(ClaimsVO claimVO) {
		String claimStatusCode = claimsDao.getClaimStatusCodeByClaimId(claimVO.getClaimId());
		Workflow workflow = claimVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(claimVO.getClaimId().toString(), Constants.CLAIM_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.CLAIM_SUBMODULE_CODE);
		}
		if (workflow != null) {
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(), maxApprovalStopNumber);
			for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
				if (finalWorkflowDetail.getApproverPersonId().equals(claimVO.getPersonId()) || finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
					claimVO.setFinalApprover(true);
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				boolean currentPerson = true;
				if (claimStatusCode.equals(Constants.CLAIM_STATUS_CODE_APPROVAL_IN_PROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (Boolean.TRUE.equals(currentPerson) && workflowDetail.getApproverPersonId().equals(claimVO.getPersonId())
								&& claimStatusCode.equals(Constants.CLAIM_STATUS_CODE_APPROVAL_IN_PROGRESS)) {
							if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
								claimVO.setIsApproved(true);
							} else {
								claimVO.setIsApproved(false);
							}
							claimVO.setIsApprover(true);
						}
					}
				}
			}
		}
	}

	@Override
	public String loadClaimReimbursement(ClaimsVO claimsVO) {
		loadClaimReimbursementData(claimsVO);			 																																																																																												
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private ClaimsVO loadClaimReimbursementData(ClaimsVO claimsVO) {
		ClaimSummary claimSummaryForOOEMAC = new ClaimSummary();
		List<ClaimSummary> claimSummary = claimsDao.loadClaimSummary(claimsVO.getClaimId());
		claimSummary.parallelStream()
		.forEach(item -> {
			if (item.getAmountRequested() != null && item.getPrevClaimsTotalAmount() != null) {
				item.setTotal(item.getAmountRequested().add(item.getPrevClaimsTotalAmount()));
			} else if(item.getPrevClaimsTotalAmount() == null) {
				item.setTotal(item.getAmountRequested());
			} else if(item.getAmountRequested() == null) {
				item.setTotal(item.getPrevClaimsTotalAmount());
			}
			BigDecimal virementPercentage = BigDecimal.ZERO;
			BigDecimal utilizationPercentage = BigDecimal.ZERO;
			if (item.getOriginalApprovedBudget() != null && item.getLatestApprovedBudget() != null && item.getOriginalApprovedBudget().intValue() > 0) {
				virementPercentage = (item.getLatestApprovedBudget().subtract(item.getOriginalApprovedBudget()).divide(item.getOriginalApprovedBudget(), 2, RoundingMode.HALF_UP).multiply(new BigDecimal(100)));
			}
			if (virementPercentage.intValue() < 0) {
				item.setVirementPercentage(BigDecimal.ZERO);
			} else {
				item.setVirementPercentage(virementPercentage);
			}
			if (item.getTotal() != null && item.getLatestApprovedBudget() != null && item.getLatestApprovedBudget().intValue() > 0) {
				utilizationPercentage = BigDecimal.valueOf((Double.parseDouble(item.getTotal().toString())/Double.parseDouble(item.getLatestApprovedBudget().toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
			}
			if (utilizationPercentage.intValue() < 0) {
				item.setUtilizationPercentage(BigDecimal.ZERO);
			} else {
				item.setUtilizationPercentage(utilizationPercentage);
			}
			item.setOriginalApprovedBudget(item.getOriginalApprovedBudget() != null ? item.getOriginalApprovedBudget() : BigDecimal.ZERO);
			item.setLatestApprovedBudget(item.getLatestApprovedBudget() != null ? item.getLatestApprovedBudget() : BigDecimal.ZERO);
			item.setAmountRequested(item.getAmountRequested() != null ? item.getAmountRequested() : BigDecimal.ZERO);
			item.setPrevClaimsTotalAmount(item.getPrevClaimsTotalAmount() != null ? item.getPrevClaimsTotalAmount() : BigDecimal.ZERO);
			item.setTotal(item.getTotal() != null ? item.getTotal() : BigDecimal.ZERO);
			setOOEAndMACvalues(item, claimSummaryForOOEMAC);
		});
		claimSummaryForOOEMAC.setBudgetCategoryCode("OOE_MAC");
		claimsVO.setSummaryOOE_MACSumValues(claimSummaryForOOEMAC);
		claimsVO.setClaimSummary(claimSummary);
		BigDecimal overHeadPercentage =  claimsDao.getOverheadPercentage(claimsVO.getClaimId());
		claimsVO.setOverHeadPercentage(overHeadPercentage);
		claimsVO.setSummaryCalculations(setClaimSummaryCalculations(claimSummary, overHeadPercentage, claimsVO.getClaimId()));
		claimsVO.setoHPercentage(setOhPercentage(overHeadPercentage));
		return claimsVO;
	}

	private ClaimSummary setOOEAndMACvalues(ClaimSummary item, ClaimSummary claimSummaryForTemplate) {
		if (item.getBudgetCategoryCode().equals("OOE") || item.getBudgetCategoryCode().equals("MAC")) {
			claimSummaryForTemplate.setOriginalApprovedBudgetForOOEMAC(claimSummaryForTemplate.getOriginalApprovedBudgetForOOEMAC().add(item.getOriginalApprovedBudget() != null ? item.getOriginalApprovedBudget(): BigDecimal.ZERO));
			claimSummaryForTemplate.setLatestApprovedBudgetForOOEMAC(claimSummaryForTemplate.getLatestApprovedBudgetForOOEMAC().add(item.getLatestApprovedBudget() != null ? item.getLatestApprovedBudget() : BigDecimal.ZERO));
			claimSummaryForTemplate.setAmountRequestedForOOEMAC(claimSummaryForTemplate.getAmountRequestedForOOEMAC().add(item.getAmountRequested() != null ? item.getAmountRequested() : BigDecimal.ZERO));
			claimSummaryForTemplate.setPrevClaimsTotalAmountForOOEMAC(claimSummaryForTemplate.getPrevClaimsTotalAmountForOOEMAC().add(item.getPrevClaimsTotalAmount() != null ? item.getPrevClaimsTotalAmount() : BigDecimal.ZERO));
			claimSummaryForTemplate.setTotalForOOEMAC(claimSummaryForTemplate.getTotalForOOEMAC().add(item.getTotal() != null ? item.getTotal() : BigDecimal.ZERO));
			claimSummaryForTemplate.setCumClaimUptoPeriodForOOEMAC(claimSummaryForTemplate.getCumClaimUptoPeriodForOOEMAC()
					.add(item.getCumClaimUptoPeriod() != null ? item.getCumClaimUptoPeriod(): BigDecimal.ZERO));
			claimSummaryForTemplate.setTotalExpenseIncuredUptoPeriodForOOEMAC(claimSummaryForTemplate.getTotalExpenseIncuredUptoPeriodForOOEMAC()
					.add(item.getTotalExpenseIncuredUptoPeriod() != null ? item.getTotalExpenseIncuredUptoPeriod(): BigDecimal.ZERO));
			claimSummaryForTemplate.setAmountForcastedForOOEMAC(claimSummaryForTemplate.getAmountForcastedForOOEMAC()
					.add(item.getAmountForcasted() != null ? item.getAmountForcasted(): BigDecimal.ZERO));
		}
		return claimSummaryForTemplate;
	}

	private String setOhPercentage(BigDecimal overHeadPercentage) {
		if (overHeadPercentage != BigDecimal.ZERO) {
			String[] oHPercentageValue = overHeadPercentage.toString().split("[.]", 0);
			String oHPercentage = oHPercentageValue[0];
			String decimalValue = oHPercentageValue[1];
			return decimalValue.equals("00") ? "(" + oHPercentage + "%)" : "(" + overHeadPercentage.toString() + "%)";
		} else {
			return "";
		}		
	}

	private Map<String, Object> setClaimSummaryCalculations(List<ClaimSummary> claimSummary, BigDecimal overHeadPercentage, Integer claimId) {
		Map<String, Object> parentMap = new HashMap<>();
		Map<String,Object> childMap = new HashMap<>();
		BigDecimal subTotalDirOriginalApproved = claimSummary.parallelStream().filter(x -> x.getOriginalApprovedBudget() != null).map(ClaimSummary::getOriginalApprovedBudget).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal subTotalDirLatestApproved = claimSummary.parallelStream().filter(x -> x.getLatestApprovedBudget() != null).map(ClaimSummary::getLatestApprovedBudget).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal subTotalDirAmountReq = claimSummary.parallelStream().filter(x -> x.getAmountRequested() != null).map(ClaimSummary::getAmountRequested).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal subTotalDirInPrevClaim = claimSummary.parallelStream().filter(x -> x.getPrevClaimsTotalAmount() != null).map(ClaimSummary::getPrevClaimsTotalAmount).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal subTotalDirAmount = subTotalDirAmountReq.add(subTotalDirInPrevClaim);
		BigDecimal subTotalDirVirment = BigDecimal.ZERO;
		BigDecimal subTotalDirUtilization = BigDecimal.ZERO;
		if (subTotalDirOriginalApproved.intValue() > 0) {
			subTotalDirVirment = BigDecimal.valueOf(((Double.parseDouble((subTotalDirLatestApproved.subtract(subTotalDirOriginalApproved)).toString())/Double.parseDouble(subTotalDirOriginalApproved.toString()))*(Double.parseDouble("100.00")))).setScale(2, RoundingMode.HALF_UP);
		}
		if (subTotalDirLatestApproved.intValue() > 0) {
			subTotalDirUtilization = BigDecimal.valueOf((Double.parseDouble(subTotalDirAmount.toString())/Double.parseDouble(subTotalDirLatestApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		childMap.put("subTotalDirOriginalApproved", subTotalDirOriginalApproved);
		childMap.put("subTotalDirLatestApproved", subTotalDirLatestApproved);
		childMap.put("subTotalDirAmountReq", subTotalDirAmountReq);
		childMap.put("subTotalDirInPrevClaim", subTotalDirInPrevClaim);
		childMap.put("subTotalDirVirment", subTotalDirVirment.intValue() < 0 ? BigDecimal.ZERO : subTotalDirVirment);
		childMap.put("subTotalDirUtilization", subTotalDirUtilization.intValue() < 0 ? BigDecimal.ZERO : subTotalDirUtilization);
		childMap.put("subTotalDirAmount", subTotalDirAmount);
		parentMap.put("subTotalOfDirectCost", new HashMap<>(childMap));
		List<ClaimSummary> claimExiList = claimSummary.stream()
				.filter(claim -> "EXI".equals(claim.getBudgetCategoryCode()))     
				.collect(Collectors.toList());
		ClaimSummary claimExi = claimExiList.isEmpty() ? new ClaimSummary() : claimExiList.get(0);
		BigDecimal qualiOriginalApproved = subTotalDirOriginalApproved.subtract(claimExi.getOriginalApprovedBudget() == null ? BigDecimal.ZERO : claimExi.getOriginalApprovedBudget());
		BigDecimal qualiLatestApproved = subTotalDirLatestApproved.subtract(claimExi.getLatestApprovedBudget() == null ?  BigDecimal.ZERO : claimExi.getLatestApprovedBudget());
		BigDecimal qualiAmountReq = subTotalDirAmountReq.subtract(claimExi.getAmountRequested() == null ? BigDecimal.ZERO : claimExi.getAmountRequested());
		BigDecimal qualiInPrevClaim = subTotalDirInPrevClaim.subtract(claimExi.getPrevClaimsTotalAmount() == null ? BigDecimal.ZERO : claimExi.getPrevClaimsTotalAmount());
		BigDecimal qualiDirAmount = qualiAmountReq.add(qualiInPrevClaim);
		BigDecimal qualiVirment = BigDecimal.ZERO;
		BigDecimal qualiUtilization = BigDecimal.ZERO;
		if (qualiOriginalApproved.intValue() > 0) {
			qualiVirment = BigDecimal.valueOf(((Double.parseDouble((qualiLatestApproved.subtract(qualiOriginalApproved)).toString())/Double.parseDouble(qualiOriginalApproved.toString()))*(Double.parseDouble("100.00")))).setScale(2, RoundingMode.HALF_UP);
		}
		if (qualiLatestApproved.intValue() > 0) {
			qualiUtilization = BigDecimal.valueOf((Double.parseDouble(qualiDirAmount.toString())/Double.parseDouble(qualiLatestApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		childMap.clear();
		childMap.put("qualiOriginalApproved", qualiOriginalApproved);
		childMap.put("qualiLatestApproved", qualiLatestApproved);
		childMap.put("qualiAmountReq", qualiAmountReq);
		childMap.put("qualiInPrevClaim", qualiInPrevClaim);
		childMap.put("qualiDirAmount", qualiDirAmount);
		childMap.put("qualiVirment", qualiVirment.intValue() < 0 ? BigDecimal.ZERO : qualiVirment);
		childMap.put("qualiUtilization", qualiUtilization.intValue() < 0 ? BigDecimal.ZERO : qualiUtilization);
		parentMap.put("qualiApprovedDirectCost", new HashMap<>(childMap));
		childMap.clear();	
		String awardNumber = null;
		BigDecimal migratedReimPrevIdcAmt = BigDecimal.ZERO;
		BigDecimal prevAdjustedOverheadTotal = null;
		BigDecimal overHeadOriginalApproved = BigDecimal.ZERO;
		BigDecimal overHeadLatestApproved = BigDecimal.ZERO;
		if (claimSummary != null && !claimSummary.isEmpty()) {
			awardNumber = claimSummary.get(0).getClaim().getAwardNumber();
			prevAdjustedOverheadTotal = claimsDao.getAdjustedIndirectCostInPrevClaims(claimId, awardNumber);
			migratedReimPrevIdcAmt = claimSummary.get(0).getClaim().getMigratedReimPrevIdcAmt() == null ? BigDecimal.ZERO : claimSummary.get(0).getClaim().getMigratedReimPrevIdcAmt();	
			overHeadOriginalApproved = claimSummary.get(0).getClaim().getIdcOrginalAmount() == null ? BigDecimal.ZERO : claimSummary.get(0).getClaim().getIdcOrginalAmount();
			overHeadLatestApproved = claimSummary.get(0).getClaim().getIdcLatestAmount() == null ? BigDecimal.ZERO : claimSummary.get(0).getClaim().getIdcLatestAmount();
		}	
		BigDecimal overHeadAmountReq = claimsDao.getAdjustedIndirectCost(claimId);
		BigDecimal overHeadInPrevClaim =  (prevAdjustedOverheadTotal != null ? prevAdjustedOverheadTotal : BigDecimal.ZERO).add(migratedReimPrevIdcAmt);
		BigDecimal overHeadDirAmount = overHeadAmountReq.add(overHeadInPrevClaim == null ? BigDecimal.ZERO : overHeadInPrevClaim);
		BigDecimal overHeadVirment = BigDecimal.ZERO;
		BigDecimal overHeadUtilization = BigDecimal.ZERO;
		if (overHeadOriginalApproved.intValue() > 0) {
			overHeadVirment = BigDecimal.valueOf(((Double.parseDouble((overHeadLatestApproved.subtract(overHeadOriginalApproved)).toString())/Double.parseDouble(overHeadOriginalApproved.toString()))*(Double.parseDouble("100.00")))).setScale(2, RoundingMode.HALF_UP);
		}
		if (overHeadLatestApproved.intValue() > 0) {
			overHeadUtilization = BigDecimal.valueOf((Double.parseDouble(overHeadDirAmount.toString())/Double.parseDouble(overHeadLatestApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		childMap.put("overHeadOriginalApproved", overHeadOriginalApproved);
		childMap.put("overHeadLatestApproved", overHeadLatestApproved);
		childMap.put("overHeadAmountReq", overHeadAmountReq);
		childMap.put("overHeadInPrevClaim", overHeadInPrevClaim);
		childMap.put("overHeadDirAmount", overHeadDirAmount);
		childMap.put("overHeadVirment", overHeadVirment.intValue() < 0 ? BigDecimal.ZERO : overHeadVirment);
		childMap.put("overHeadUtilization", overHeadUtilization.intValue() < 0 ? BigDecimal.ZERO : overHeadUtilization);
		parentMap.put("overHeads", new HashMap<>(childMap));
		childMap.clear();
		BigDecimal subTotalInDirOriginalApproved = overHeadOriginalApproved != null ? overHeadOriginalApproved : BigDecimal.ZERO;
		BigDecimal subTotalInDirLatestApproved = overHeadLatestApproved != null ? overHeadLatestApproved :BigDecimal.ZERO;
		BigDecimal subTotalInDirAmountReq = overHeadAmountReq != null ? overHeadAmountReq : BigDecimal.ZERO;
		BigDecimal subTotalInDirInPrevClaim = overHeadInPrevClaim != null ? overHeadInPrevClaim : BigDecimal.ZERO;
		BigDecimal subTotalInDirDirAmount = subTotalInDirAmountReq.add(subTotalInDirInPrevClaim == null ? BigDecimal.ZERO : subTotalInDirInPrevClaim);
		BigDecimal subTotalInDirVirment = BigDecimal.ZERO;
		BigDecimal subTotalInDirUtilization = BigDecimal.ZERO;
		if (subTotalInDirOriginalApproved.intValue() > 0) {
			subTotalInDirVirment = BigDecimal.valueOf(((Double.parseDouble((subTotalInDirLatestApproved.subtract(subTotalInDirOriginalApproved)).toString())/Double.parseDouble(subTotalInDirOriginalApproved.toString()))*(Double.parseDouble("100.00")))).setScale(2, RoundingMode.HALF_UP);
		}
		if (subTotalInDirLatestApproved.intValue() > 0) {
			subTotalInDirUtilization = BigDecimal.valueOf((Double.parseDouble(subTotalInDirDirAmount.toString())/Double.parseDouble(subTotalInDirLatestApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		childMap.put("subTotalInDirOriginalApproved", subTotalInDirOriginalApproved);
		childMap.put("subTotalInDirLatestApproved", subTotalInDirLatestApproved);
		childMap.put("subTotalInDirAmountReq", subTotalInDirAmountReq);
		childMap.put("subTotalInDirInPrevClaim", subTotalInDirInPrevClaim);
		childMap.put("subTotalInDirDirAmount",  subTotalInDirDirAmount);
		childMap.put("subTotalInDirVirment", subTotalInDirVirment.intValue() < 0 ? BigDecimal.ZERO : subTotalInDirVirment);
		childMap.put("subTotalInDirUtilization", subTotalInDirUtilization.intValue() < 0 ? BigDecimal.ZERO : subTotalInDirUtilization);
		parentMap.put("subTotalOfInDirectCost", new HashMap<>(childMap));
		childMap.clear();
		BigDecimal totalExpOriginalApproved = subTotalDirOriginalApproved.add(overHeadOriginalApproved);
		BigDecimal totalExpLatestApproved = subTotalDirLatestApproved.add(overHeadLatestApproved);
		BigDecimal totalExpAmountReq = subTotalDirAmountReq.add(overHeadAmountReq);
		BigDecimal totalExpInPrevClaim = subTotalDirInPrevClaim.add(overHeadInPrevClaim == null ? BigDecimal.ZERO : overHeadInPrevClaim);
		BigDecimal totalExpDirAmount = totalExpAmountReq.add(totalExpInPrevClaim);
		BigDecimal totalExpVirment = BigDecimal.ZERO;
		BigDecimal totalExpUtilization = BigDecimal.ZERO;
		BigDecimal totalDirIndirectPercentage = BigDecimal.ZERO;
		if (totalExpOriginalApproved.intValue() > 0) {
			totalExpVirment = BigDecimal.valueOf(((Double.parseDouble((totalExpLatestApproved.subtract(totalExpOriginalApproved)).toString())/Double.parseDouble(totalExpOriginalApproved.toString()))*(Double.parseDouble("100.00")))).setScale(2, RoundingMode.HALF_UP);
		}
		if (totalExpLatestApproved.intValue() > 0) {
			totalExpUtilization = BigDecimal.valueOf((Double.parseDouble(totalExpDirAmount.toString())/Double.parseDouble(totalExpLatestApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		if (totalExpOriginalApproved.intValue() > 0) {
			totalDirIndirectPercentage = BigDecimal.valueOf((Double.parseDouble(totalExpAmountReq.toString())/Double.parseDouble(totalExpOriginalApproved.toString()))*(Double.parseDouble("100.00"))).setScale(2, RoundingMode.HALF_UP);
		}
		childMap.put("totalExpOriginalApproved", totalExpOriginalApproved);
		childMap.put("totalExpLatestApproved", totalExpLatestApproved);
		childMap.put("totalExpAmountReq", totalExpAmountReq);
		childMap.put("totalExpInPrevClaim", totalExpInPrevClaim);
		childMap.put("totalExpDirAmount", totalExpDirAmount);
		childMap.put("totalExpVirment", totalExpVirment.intValue() < 0 ? BigDecimal.ZERO : totalExpVirment);
		childMap.put("totalExpUtilization", totalExpUtilization.intValue() < 0 ? BigDecimal.ZERO : totalExpUtilization);
		childMap.put("totalDirIndirectPercentage", totalDirIndirectPercentage.intValue() < 0 ? BigDecimal.ZERO : totalDirIndirectPercentage);
		parentMap.put("totalExpenditure", new HashMap<>(childMap));
		parentMap.put("totalAmount", totalExpAmountReq);
		return parentMap;		
	}

	@Override
	public String loadClaimDetailBreakDown(ClaimsVO claimsVO) {
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.CLAIM_MAINTAIN_DETAILED_BREAKDOWN, claimsVO.getLeadUnitNumber());
		if(isPersonHasPermission.equals(Boolean.FALSE))
			return commonDao.convertObjectToJSON(claimsVO);
		List<ClaimSummaryDetails>  initialSummaryDetails =  claimsDao.loadClaimDetailBreakDown(claimsVO.getClaimId());
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> summary.getBudgetCategoryCode().equals("EOM") || summary.getBudgetCategoryCode().equals("RSS")).forEach(summary -> {
			try {
				commonDao.detachEntityFromSession(summary);
				if (summary.getManpowerPayrollId() != null) {
					summary.setQualifyingCost(summary.getEncryptedAmount() != null ? new BigDecimal(excelityService.decryptAESData(summary.getEncryptedAmount())) : null);
					summary.setTotalAmount(summary.getTotalAmount() == null ? summary.getQualifyingCost() : summary.getTotalAmount());
					summary.setAdjustedTotal(summary.getAdjustedTotal() == null ? summary.getQualifyingCost() : summary.getAdjustedTotal());
				} else {
					setInvolvmentDate(summary);
				}
				summary.setDescriptionOfExpenditure(summary.getAwardExpenseTransaction().getRemarks());
				summary.setExpenseCode(summary.getAwardExpenseTransaction().getInternalOrderCode().substring(15));
				claimsDao.getBudgetDetails(summary);
			} catch (Exception e) {
				logger.error("error while decrypting amount", e);
			}
		});
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> !summary.getBudgetCategoryCode().equals("EOM")
				&& !summary.getBudgetCategoryCode().equals("RSS")).forEach(summary -> {
			summary.setDescriptionOfExpenditure(summary.getAwardExpenseTransaction().getRemarks());
			commonDao.detachEntityFromSession(summary);
			claimsDao.getBudgetDetails(summary);
		});
		BigDecimal overHeadPercentage =  claimsDao.getOverheadPercentage(claimsVO.getClaimId());
	getManpowerdetails(initialSummaryDetails);
		List<LinkedHashMap<Object, Object>> finalResult = new ArrayList<>();
		List<BigDecimal> quarterTotalAmount = new ArrayList<>();
		List<BigDecimal> quarterAdjustedTotal = new ArrayList<>();
		LinkedHashMap<String, List<ClaimSummaryDetails>> summaryGroups = initialSummaryDetails.stream()
				.sorted(Comparator.comparing(ClaimSummaryDetails::getBudgetCategoryCode))
				.collect(Collectors.groupingBy(summaryDetails -> summaryDetails.getClaimSummary().getBudgetCategoryCode(),LinkedHashMap::new,Collectors.toList()));
		summaryGroups.values().forEach(summaryBudgetDetails -> {
			LinkedHashMap<Object, List<ClaimSummaryDetails>> summaryGroupsInternal = summaryBudgetDetails.stream()
					.sorted(Comparator.comparing(summaryDetails -> summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes()))
					.collect(Collectors.groupingBy(summaryDetails -> summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes(),LinkedHashMap::new,Collectors.toList()));
			summaryGroupsInternal.values().forEach(summaryDetailsList -> {
				LinkedHashMap<Object, Object> result = new LinkedHashMap<>();
				summaryDetailsList.stream().sorted(Comparator.comparing(ClaimSummaryDetails::getClaimDetailsId)).collect(Collectors.toList()).forEach(summaryDetails -> {
					result.put("budgetCategory", summaryDetails.getClaimSummary().getBudgetCategory());
					result.put("internalOrderCode", summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes());
					result.put("approvedBudget", summaryDetails.getApprovedBudget());
					if (summaryDetails.getApprovedHeadCount() != null)
						result.put("approvedHeadCount", summaryDetails.getApprovedHeadCount());
					if (summaryDetails.getActualHeadCount() != null)
						result.put("actualHeadCount", summaryDetails.getActualHeadCount());
					result.put("awardNumber", summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getAwardNumber() : summaryDetails.getClaimSummary().getClaim().getAwardNumber());
					if (Boolean.TRUE.equals(claimsVO.getIsExcelExport())) {
						if (summaryDetails.getApprovedHeadCount() != null)
							result.put("approvedHeadCountData", summaryDetails.getClaimSummary().getBudgetCategory().getCode().equals("EOM") && summaryDetails.getApprovedHeadCount() != null ? "Approved HeadCount : "+ summaryDetails.getApprovedHeadCount().toString() : "");
						if (summaryDetails.getActualHeadCount() != null)
							result.put("actualHeadCountData", summaryDetails.getClaimSummary().getBudgetCategory().getCode().equals("EOM") && summaryDetails.getActualHeadCount() != null ? "Actual HeadCount : "+ summaryDetails.getActualHeadCount().toString() : "");
						result.put("approvedBudgetData", summaryDetails.getApprovedBudget() != null ? new StringBuilder("Approved Budget : ").append(new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL).format(summaryDetails.getApprovedBudget())).toString() : "");
					}
				});
				List<Map<Object, Object>> finalTransactionResult = new ArrayList<>();
				if (Boolean.TRUE.equals(claimsVO.getIsExcelExport())) {
					List<ClaimSummaryDetails> claimSummaryDetails = new ArrayList<>();
					for (ClaimSummaryDetails summaryDetail : summaryDetailsList) {
						ClaimSummaryDetails newSummaryDetail = new ClaimSummaryDetails();
						BeanUtils.copyProperties(summaryDetail, newSummaryDetail);
						if (Boolean.FALSE.equals(summaryDetail.getIsExcludedFlag())) {
							claimSummaryDetails.add(summaryDetail);
						}
					}
					prepareClaimSummaryDetails(claimSummaryDetails, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
					setgroupedTransactionsDetails(claimSummaryDetails, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
				} else {
					prepareClaimSummaryDetails(summaryDetailsList, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
					setgroupedTransactionsDetails(summaryDetailsList, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
				}
			});
		});
		Map<Object, Object> calculationParentMap = new HashMap<>();
		Map<Object, Object> calculationChildMap = new HashMap<>();
		BigDecimal quarterTotal = quarterTotalAmount.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
		BigDecimal quarterAdjTotal = quarterAdjustedTotal.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
		calculationChildMap.put("subTotalDirectTotalAmount", quarterTotal);
		calculationChildMap.put("subTotalDirectAdjTotalAmount", quarterAdjTotal);
		calculationParentMap.put("subTotalDirectCost", new HashMap<>(calculationChildMap));
		calculationChildMap.clear();
		BigDecimal inDirectTotalAmount = quarterTotal.multiply(overHeadPercentage.divide(new BigDecimal(100)));
		BigDecimal inDirectAdjTotalAmount =  claimsDao.getAdjustedIndirectCost(claimsVO.getClaimId());
		calculationChildMap.put("inDirectTotalAmount", inDirectTotalAmount);
		calculationChildMap.put("inDirectAdjTotalAmount", inDirectAdjTotalAmount);
		calculationParentMap.put("inDirectCost", new HashMap<>(calculationChildMap));
		calculationChildMap.clear();
		calculationChildMap.put("expenditureTotalAmount", quarterTotal.add(inDirectTotalAmount));
		calculationChildMap.put("expenditureAdjTotalAmount", quarterAdjTotal.add(inDirectAdjTotalAmount));
		calculationParentMap.put("totalExpenditure", new HashMap<>(calculationChildMap));
		calculationChildMap.clear();
		claimsVO.setClaimSummaryDetails(finalResult);
		claimsVO.setClaimSummaryCalculations(calculationParentMap);
		claimsVO.setOverHeadPercentage(overHeadPercentage);
		claimsVO.setoHPercentage(setOhPercentageDetailBreakDown(overHeadPercentage));		
		return commonDao.convertObjectToJSON(claimsVO);
	}
	
	private String setOhPercentageDetailBreakDown(BigDecimal overHeadPercentage) {
		if (overHeadPercentage != BigDecimal.ZERO) {
			String[] oHPercentageValue = overHeadPercentage.toString().split("[.]", 0);
			String oHPercentage = oHPercentageValue[0];
			String decimalValue = oHPercentageValue[1];
			return decimalValue.equals("00") ? oHPercentage : overHeadPercentage.toString();
		} 
		return overHeadPercentage.toString();
	}

	private void setInvolvmentDate(ClaimSummaryDetails summary) {
		try {
			if (summary.getAwardExpenseTransaction().getRemarks() != null && !summary.getAwardExpenseTransaction().getRemarks().isEmpty() && summary.getAwardExpenseTransaction().getRemarks().length() >= 18) {
				String involvementDate = new StringBuilder(new StringBuilder(summary.getAwardExpenseTransaction().getRemarks()).reverse().substring(0, 17)).reverse().toString();
				String invlostart = involvementDate.substring(0,8);
				String invloend = involvementDate.substring(9, 17);
				SimpleDateFormat dateFormat = new SimpleDateFormat("ddMMyyyy");
				SimpleDateFormat finalDateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
				SimpleDateFormat finalDateFormatForTemplate = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT_CLAIM);
				Date involvementStartDate = null;
				Date involvementEndDate = null;
				Date involvementStartDateForTemplate = null;
				Date involvementEndDateForTemplate = null;
				if (invlostart != null && invloend != null) {
					involvementStartDate = (Date) dateFormat.parse(invlostart);
					String involvementStartDateString = String.valueOf(finalDateFormat.format(new Date(involvementStartDate.getTime())));
					involvementEndDate = (Date) dateFormat.parse(invloend);
					String involvementEndDateString = String.valueOf(finalDateFormat.format(new Date(involvementEndDate.getTime())));
					summary.setInvolvementPeriod(involvementStartDateString + "-" + involvementEndDateString);
					involvementStartDateForTemplate = (Date) dateFormat.parse(invlostart);
					involvementEndDateForTemplate = (Date) dateFormat.parse(invloend);
					String involvementStartDateStringForTemplate = String.valueOf(finalDateFormatForTemplate.format(new Date(involvementStartDateForTemplate.getTime())));
					String involvementEndDateStringForTemplate = String.valueOf(finalDateFormatForTemplate.format(new Date(involvementEndDateForTemplate.getTime())));
					summary.setInvolvementPeriodForTemplate(involvementStartDateStringForTemplate + " to " + involvementEndDateStringForTemplate);
					logger.info("InvolvementPeriod For Template: {}", summary.getInvolvementPeriodForTemplate());
				}
			}
		} catch (Exception e) {
			logger.error("error while getting involvement date {}", e.getMessage());
		}
	}

	private void prepareClaimSummaryDetails(List<ClaimSummaryDetails> summaryDetailsList, LinkedHashMap<Object, Object> result, List<BigDecimal> quarterTotalAmount, 
			List<BigDecimal> quarterAdjustedTotal, List<Map<Object, Object>> finalTransactionResult, List<LinkedHashMap<Object, Object>> finalResult) {	
		LinkedHashMap<String, List<ClaimSummaryDetails>> internalTransactions = summaryDetailsList.stream()
				.sorted(Comparator.comparing(ClaimSummaryDetails::getFullNameEOM).reversed())
				.collect(Collectors.groupingBy(ClaimSummaryDetails::getFullNameEOM,LinkedHashMap::new,Collectors.toList()));
		Set<String> lineItems = new HashSet<>();
		StringBuilder lineItemDescription = new StringBuilder();
		internalTransactions.values().forEach(transactions -> {
			Set<String> jobDescription = new HashSet<>();
			Map<Object, Object> transactionResult = new HashMap<>();			
			transactions = transactions.stream().sorted(Comparator.comparing(ClaimSummaryDetails::getClaimDetailsId)).collect(Collectors.toList());		
			transactionResult.put("manpowerFullName", transactions.get(0).getFullNameEOM().isEmpty() ? transactions.get(0).getBudgetCategoryCode().equals("EOM") || transactions.get(0).getBudgetCategoryCode().equals("RSS") ? "Others" : null : transactions.get(0).getFullNameEOM());
			transactionResult.put("manpowerJobDescription", !transactions.get(0).getJobDescription().isEmpty() && transactions.get(0).getBudgetCategoryCode().equals("EOM") ?  transactions.get(0).getJobDescription() : null);
			transactionResult.put("transactions", transactions);
			Manpower manpower = null;
			if(transactions.get(0).getPersonId() != null ) {
				manpower = manpowerDao.fetchManpowerPersonDetail(null, transactions.get(0).getPersonId());
				if(manpower != null) {
					try {
						manpower.setDecryptedCitizenShip(manpower.getCitizenship() != null ? excelityService.decryptAESData(manpower.getCitizenship()) : null);
					} catch (Exception e) {
						logger.error("prepareClaimSummaryDetails {}", e.getMessage());
					}
				}
			}
			transactionResult.put("manpower", transactions.get(0).getFullNameEOM().isEmpty() ? transactions.get(0).getBudgetCategoryCode().equals("EOM") ||
					transactions.get(0).getBudgetCategoryCode().equals("RSS") ? null : null : manpower);
			finalTransactionResult.add(transactionResult);
			if (transactions != null && !transactions.isEmpty() && transactions.get(0).getBudgetCategoryCode().equals("EOM") ) {
				if (!transactions.get(0).getJobDescription().isEmpty()) {
					jobDescription.add(transactions.get(0).getJobDescription());
				}
				lineItems.addAll(jobDescription);
			} else {
				lineItems.addAll(transactions.stream().map(ClaimSummaryDetails::getDescriptionOfExpenditure).collect(Collectors.toSet()));
			}
		});
		result.put("lineItemDescription", setExpenditureLineItemBasedOnLineItem(lineItems, lineItemDescription));
	}

	private String setExpenditureLineItemBasedOnLineItem(Set<String> lineItems,
			StringBuilder lineItemDescription) {
		for(String lineItem : lineItems) {
			lineItemDescription.append(lineItem).append(",");
		}
		return lineItemDescription != null && lineItemDescription.length() != 0 ? lineItemDescription.toString().substring(0, lineItemDescription.length() -1) : "";
	}

	private void setgroupedTransactionsDetails(List<ClaimSummaryDetails> summaryDetailsList, LinkedHashMap<Object, Object> result, List<BigDecimal> quarterTotalAmount, List<BigDecimal> quarterAdjustedTotal, List<Map<Object, Object>> finalTransactionResult, List<LinkedHashMap<Object, Object>> finalResult) {
		BigDecimal quarterTotal = summaryDetailsList.parallelStream().filter(x -> x.getTotalAmount() != null && 
				(x.getPrevAdjustedSummaryDetId() == null || x.getPrevExcludedSummaryDetId() != null)).map(ClaimSummaryDetails::getTotalAmount).reduce(BigDecimal.ZERO,BigDecimal::add);
		result.put("quarterTotalAmount", quarterTotal);	
		quarterTotalAmount.add(quarterTotal);
		BigDecimal quarterAdjusted = summaryDetailsList.parallelStream().filter(x -> x.getAdjustedTotal() != null && x.getIsExcludedFlag().equals(false))
				.map(ClaimSummaryDetails::getAdjustedTotal).reduce(BigDecimal.ZERO,BigDecimal::add);
		quarterAdjustedTotal.add(quarterAdjusted);
		result.put("quarterAdjustedTotal", quarterAdjusted);
		result.put(GROUPED_TRANSACTIONS, finalTransactionResult);					
		finalResult.add(result);
	}

	private void getManpowerdetails(List<ClaimSummaryDetails> initialSummaryDetails) {
		Set<String> internalOrderCode = initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> Stream.of("RSS")				
			      .anyMatch(s -> s.equalsIgnoreCase(summary.getBudgetCategoryCode()))).map(summary -> summary.getAwardExpenseTransaction().getInternalOrderCode()).collect(Collectors.toSet());
		Set<Integer> payrollId = initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> Stream.of("EOM","RSS")
			      .anyMatch(s -> s.equalsIgnoreCase(summary.getBudgetCategoryCode()))).map(summary -> summary.getManpowerPayrollId()).collect(Collectors.toSet());
		List<Object[]> summaryDetailsWithManpowerPayroll = claimsDao.getManpowerdetailsByPayrollId(payrollId);
		List<ClaimSummaryDetails> manpowerSummaryDetailsPayroll = new ArrayList<>();
		summaryDetailsWithManpowerPayroll.stream().forEach(manPower ->{
			ClaimSummaryDetails manpowerSummaryDetail = new ClaimSummaryDetails();
			Object fullName = manPower[0];
			Object budgetReferenceNumber = manPower[1];
			Object approvedHeadCount = manPower[2];
			Object awardManpowerId = manPower[3];
			Object personId = manPower[4];
			Object glAccount = manPower[5];
			Object payroll = manPower[6];
			Object payElement = manPower[7];
			Object chargeStartDate = manPower[8];
			Object chargeEndDate = manPower[9];
			Object planStartDate = manPower[10];
			Object planEndDate = manPower[11];
			Object jobDescriptionCode = manPower[12];	
			if (chargeStartDate != null && chargeEndDate != null) {			
				manpowerSummaryDetail.setInvolvementPeriod(generateInvolvmentPeriodForManpower(chargeStartDate, chargeEndDate));
				manpowerSummaryDetail.setInvolvementPeriodForTemplate(generateInvolvementPeriodForTemplate(chargeStartDate, chargeEndDate));
			} else if (planStartDate != null && planEndDate != null && manpowerSummaryDetail.getInvolvementPeriod() == null) {				
				manpowerSummaryDetail.setInvolvementPeriod(generateInvolvmentPeriodForManpower(planStartDate, planEndDate));
				manpowerSummaryDetail.setInvolvementPeriodForTemplate(generateInvolvementPeriodForTemplate(planStartDate, planEndDate));
			}
			manpowerSummaryDetail.setFullNameEOM(fullName != null ? fullName.toString() : "");
			manpowerSummaryDetail.setInternalOrderCode(budgetReferenceNumber != null ? budgetReferenceNumber.toString() : null);
			manpowerSummaryDetail.setBudgetCategoryCode(budgetReferenceNumber != null ? budgetReferenceNumber.toString().substring(15,18) : null);
			manpowerSummaryDetail.setApprovedHeadCount(approvedHeadCount != null ? new BigDecimal(approvedHeadCount.toString()).intValue() : 0);
			Integer actualHeadCount = manpowerDao.countOfActiveManpowerResource((Integer) awardManpowerId);
			manpowerSummaryDetail.setActualHeadCount(actualHeadCount != null ? Integer.parseInt(actualHeadCount.toString()) : 0);
			manpowerSummaryDetail.setPersonId(personId != null ? personId.toString() : null);
			manpowerSummaryDetail.setGlAccountCode(glAccount != null ? glAccount.toString() : "");
			manpowerSummaryDetail.setManpowerPayrollId(payroll != null ? Integer.parseInt(payroll.toString()) : 0);
			manpowerSummaryDetail.setDescriptionOfExpenditure(payElement != null ? payElement.toString() : null);
			manpowerSummaryDetail.setJobDescription(jobDescriptionCode != null ? jobDescriptionCode.toString() : "");
			manpowerSummaryDetailsPayroll.add(manpowerSummaryDetail);
		});		
		List<Object[]> summaryDetailsWithManpower = claimsDao.getManpowerdetails(internalOrderCode);
		List<ClaimSummaryDetails> manpowerSummaryDetails = new ArrayList<>();
		summaryDetailsWithManpower.stream().forEach(manPower ->{
			ClaimSummaryDetails manpowerSummaryDetail = new ClaimSummaryDetails();
			Object fullName = manPower[0];
			Object budgetReferenceNumber = manPower[1];
			Object approvedHeadCount = manPower[2];
			Object awardManpowerId = manPower[3];
			Object personId = manPower[4];
			Object glAccount = manPower[5];
			manpowerSummaryDetail.setFullNameEOM(fullName != null ? fullName.toString() : "");
			manpowerSummaryDetail.setInternalOrderCode(budgetReferenceNumber != null ? budgetReferenceNumber.toString() : null);
			manpowerSummaryDetail.setApprovedHeadCount(approvedHeadCount != null ? new BigDecimal(approvedHeadCount.toString()).intValue() : 0);
			Integer actualHeadCount = manpowerDao.countOfActiveManpowerResource((Integer) awardManpowerId);
			manpowerSummaryDetail.setActualHeadCount(actualHeadCount != null ? Integer.parseInt(actualHeadCount.toString()) : 0);
			manpowerSummaryDetail.setPersonId(personId != null ? personId.toString() : "");
			manpowerSummaryDetail.setGlAccountCode(glAccount != null ? glAccount.toString() : "");
			manpowerSummaryDetails.add(manpowerSummaryDetail);
		});
		try {
			Map<Integer, List<ClaimSummaryDetails>> groupedManpowerSummaryEOM = manpowerSummaryDetailsPayroll.stream()
					.collect(Collectors.groupingBy(ClaimSummaryDetails::getManpowerPayrollId));
			initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summaryDetails -> summaryDetails.getBudgetCategoryCode().equals("EOM") || summaryDetails.getBudgetCategoryCode().equals("RSS")).filter(manpowerResource -> groupedManpowerSummaryEOM.containsKey(manpowerResource.getManpowerPayrollId())).forEach(manpowerResource -> {
				manpowerResource.setActualHeadCount(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getActualHeadCount());
				manpowerResource.setApprovedHeadCount(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getApprovedHeadCount());
				manpowerResource.setPersonId(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getPersonId());
				manpowerResource.setFullNameEOM(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getFullNameEOM());
				manpowerResource.setJobDescription(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getJobDescription());
				manpowerResource.setDescriptionOfExpenditure(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getDescriptionOfExpenditure());
				manpowerResource.setInvolvementPeriod(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getInvolvementPeriod());
				manpowerResource.setInvolvementPeriodForTemplate(groupedManpowerSummaryEOM.get(manpowerResource.getManpowerPayrollId()).stream().filter(summary -> summary.getInternalOrderCode().equals(manpowerResource.getAwardExpenseTransaction().getInternalOrderCode())).findFirst().get().getInvolvementPeriodForTemplate());
			});
		} catch (Exception e) {
			logger.error("error while setting manpower payroll data into claim details", e);
		}
		Map<String, List<ClaimSummaryDetails>> groupedManpowerSummaryRSS = manpowerSummaryDetails.stream()
				.collect(Collectors.groupingBy(ClaimSummaryDetails::getPersonId));
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null && summary.getManpowerPayrollId() == null && summary.getBudgetCategoryCode().equals("RSS"))
		.filter(summaryDetails -> (summaryDetails.getAwardExpenseTransaction().getTransactionReferenceNumber() != null && summaryDetails.getAwardExpenseTransaction().getVendorCode() != null))
		.filter(summaryDetails -> summaryDetails.getAwardExpenseTransaction().getTransactionReferenceNumber().substring(0, 3).equals("PTC"))
		.filter(summaryDetails -> groupedManpowerSummaryRSS.containsKey(summaryDetails.getAwardExpenseTransaction().getVendorCode().substring(1))).forEach(summaryDetails -> {
			summaryDetails.setApprovedHeadCount(groupedManpowerSummaryRSS.get(summaryDetails.getAwardExpenseTransaction().getVendorCode().substring(1)).get(0).getApprovedHeadCount());
			summaryDetails.setPersonId(groupedManpowerSummaryRSS.get(summaryDetails.getAwardExpenseTransaction().getVendorCode().substring(1)).get(0).getPersonId());
			summaryDetails.setFullNameEOM(groupedManpowerSummaryRSS.get(summaryDetails.getAwardExpenseTransaction().getVendorCode().substring(1)).get(0).getFullNameEOM());
		});	
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summaryDetails -> summaryDetails.getBudgetCategoryCode().equals("RSS"))
		.filter(summaryDetails -> (summaryDetails.getAwardExpenseTransaction().getTransactionReferenceNumber() != null && summaryDetails.getAwardExpenseTransaction().getVendorCode() != null))
		.filter(summaryDetails -> summaryDetails.getAwardExpenseTransaction().getTransactionReferenceNumber().substring(0, 3).equals("PTC"))
		.forEach(summaryDetails -> {	
			setInvolvmentDate(summaryDetails);			  
		});	
	}

	private String generateInvolvmentPeriodForManpower(Object startDate, Object endDate) {
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
			SimpleDateFormat finalDateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
			Date startDateIntial = dateFormat.parse(startDate.toString().substring(0, 10).replace("-", "/"));
			String startDateString =  String.valueOf(finalDateFormat.format(new Date(startDateIntial.getTime())));
			Date planEndIntial = dateFormat.parse(endDate.toString().substring(0, 10).replace("-", "/"));
			String endDateString =  String.valueOf(finalDateFormat.format(new Date(planEndIntial.getTime())));
			return startDateString + "-" + endDateString;
		} catch (Exception e) {
			logger.error("error while parsing date", e);
		}
		return null;		
	}
	
	private String generateInvolvementPeriodForTemplate(Object startDate, Object endDate) {
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
			SimpleDateFormat finalDateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT_CLAIM);
			Date startDateIntial = dateFormat.parse(startDate.toString().substring(0, 10).replace("-", "/"));
			String startDateString =  String.valueOf(finalDateFormat.format(new Date(startDateIntial.getTime())));
			Date planEndIntial = dateFormat.parse(endDate.toString().substring(0, 10).replace("-", "/"));
			String endDateString =  String.valueOf(finalDateFormat.format(new Date(planEndIntial.getTime())));
			return startDateString + " to " + endDateString;
		} catch (Exception e) {
			logger.error("generateInvolvementPeriodForTemplate", e);
		}
		return null;
	}

	@Override
	public String saveOrUpdateClaimBreakDown(ClaimsVO claimsVO) {
		try {
			ClaimSummaryDetails claimSummaryDetail = claimsVO.getClaimSummaryDetail();
			claimsDao.updateClaimBreakDown(claimSummaryDetail);
			if (claimSummaryDetail.getAdjustedTotal().compareTo(claimSummaryDetail.getTotalAmount()) != 0){
				BigDecimal newAdjustedTotal = claimSummaryDetail.getTotalAmount().subtract(claimSummaryDetail.getAdjustedTotal());
				ClaimSummaryDetails prevAdjustedSummary = claimsDao.getClaimPrevSummaryDetailById(claimSummaryDetail.getClaimDetailsId());				
				if (prevAdjustedSummary == null) {
					ClaimSummaryDetails adjustedSummary = claimsDao.getClaimSummaryDetailById(claimSummaryDetail.getClaimDetailsId());
					ClaimSummaryDetails newAdjustedSummary = new ClaimSummaryDetails();
					BeanUtils.copyProperties(adjustedSummary, newAdjustedSummary);
					newAdjustedSummary.setPrevAdjustedSummaryDetId(adjustedSummary.getClaimDetailsId());
					newAdjustedSummary.setClaimDetailsId(null);
					newAdjustedSummary.setIsExcludedFlag(true);
					newAdjustedSummary.setTotalAmount(newAdjustedTotal);
					newAdjustedSummary.setAdjustedTotal(newAdjustedTotal);	
					claimsDao.saveOrUpdateClaimBreakDown(newAdjustedSummary);
				} else {
					prevAdjustedSummary.setTotalAmount(newAdjustedTotal);
					prevAdjustedSummary.setAdjustedTotal(newAdjustedTotal);
					prevAdjustedSummary.setUpdateUser(claimSummaryDetail.getUpdateUser());
					prevAdjustedSummary.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					claimsDao.saveOrUpdateClaimBreakDown(prevAdjustedSummary);					
				}	
			} else {
				claimsDao.deleteSummaryDetailByPrevSummayId(claimSummaryDetail.getClaimDetailsId());
			}
			claimsVO.setClaimId(claimSummaryDetail.getClaimId());
			BigDecimal amountRequested = claimsDao.getClaimSummaryAmountReqForAll(claimsVO.getClaimId(), claimSummaryDetail.getBudgetCategoryCode());
			updateClaimSummaryAmountForBudgetCategory(claimsVO.getClaimId(), claimSummaryDetail.getBudgetCategoryCode(), amountRequested);
			updateAdjustedIndirectCostFromDetailBreakDown(claimsVO.getClaimId());			
		} catch (Exception e) {
			logger.error("error in saveOrUpdateClaimBreakDown : {}", e.getMessage());
		}		
		return loadClaimDetailBreakDown(claimsVO);
	}

	private void updateAdjustedIndirectCostFromDetailBreakDown(Integer claimId) {
		BigDecimal overHeadPercentage =  claimsDao.getOverheadPercentage(claimId);
		BigDecimal totalAmountReq = claimsDao.getTotalAmountRequestedForClaim(claimId);
		BigDecimal adjustedInDirectCost = totalAmountReq.multiply(overHeadPercentage.divide(new BigDecimal(100)));
		claimsDao.updateAdjustedIndirectCost(adjustedInDirectCost, claimId);		
	}

	private void updateClaimSummaryAmountForBudgetCategory(Integer claimId, String budgetCategoryCode, BigDecimal amountRequested) {
		ClaimSummary summary = claimsDao.getClaimSummaryByParams(claimId, budgetCategoryCode);		
		BigDecimal cumAmountUntilLastClaim = summary.getCumClaimAmountUptoClaim() == null ? BigDecimal.ZERO : summary.getCumClaimAmountUptoClaim();
		BigDecimal totalExpIncuredUptoClaim = BigDecimal.ZERO;
		BigDecimal fundBalanceAtEndOfClaim = BigDecimal.ZERO;
		BigDecimal amountReqForCurrentClaim = BigDecimal.ZERO;
		BigDecimal cumExpenseUptoPrevClaimPeriod = summary.getCumExpenseUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCumExpenseUptoPrevClaim();
		BigDecimal commitmentsAmtUptoPeriod = summary.getCommitmentsUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCommitmentsUptoPrevClaim();
		totalExpIncuredUptoClaim = cumExpenseUptoPrevClaimPeriod.add(amountRequested == null ? BigDecimal.ZERO : amountRequested);
		fundBalanceAtEndOfClaim = cumAmountUntilLastClaim.subtract(totalExpIncuredUptoClaim);
		amountReqForCurrentClaim = commitmentsAmtUptoPeriod.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted()).subtract(fundBalanceAtEndOfClaim);
		summary.setCumClaimAmountUptoClaim(cumAmountUntilLastClaim);
		summary.setCumExpenseUptoPrevClaim(cumExpenseUptoPrevClaimPeriod);
		summary.setCommitmentsUptoPrevClaim(commitmentsAmtUptoPeriod);
		summary.setAmountReqForCurrentClaim(amountReqForCurrentClaim);
		summary.setAmountRequested(amountRequested);
		claimsDao.saveOrUpdateClaimSummary(summary);
	}

	@Override
	public String saveOrUpdateClaimOverHead(ClaimsVO claimsVO) {
		claimsDao.saveOrUpdateClaimOverHead(claimsVO.getClaim());
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public String getPrevExcludedClaimSummaryDetails(ClaimsVO claimsVO) {
		List<ClaimSummaryDetails> excludedSummaryDetails = claimsDao.getPrevExcludedClaimSummaryDetails(claimsVO.getAwardNumber(), claimsVO.getInternalOrderCode(), claimsVO.getClaimId());
		excludedSummaryDetails.forEach(summary -> {
			claimsDao.getBudgetDetails(summary);
			try {
				if((summary.getBudgetCategoryCode().equals("EOM") || summary.getBudgetCategoryCode().equals("RSS")) && summary.getEncryptedAmount() != null)
					summary.setQualifyingCost(new BigDecimal(excelityService.decryptAESData(summary.getEncryptedAmount())));
			} catch (Exception e) {
				logger.error("error in getPrevExcludedClaimSummaryDetails decrypting amount : {}", e.getMessage());
			}
		});
		claimsVO.setPrevClaimSummaryDetails(excludedSummaryDetails);
		return commonDao.convertObjectToJSON(claimsVO);				 
	}
	
	@Override
	public String saveOrUpdateClaimAttachment(MultipartFile[] files, String formDataJson) {
		ClaimsVO claimsVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			claimsVO = mapper.readValue(formDataJson, ClaimsVO.class);
			switch (claimsVO.getAcType()) {
			case "I":
				addNewProtocolAttachment(files,claimsVO.getClaimAttachments(), claimsVO.getUpdateUser());
				if(claimsVO.getClaimAttachments().get(0).getTypeCode().equals("1")) {
					List<ClaimSummary> fileContents = readFileContents(claimsVO.getUpdateUser(), files[0], claimsVO.getClaimAttachments().get(0).getClaimId());
					claimsVO.setClaimId(claimsVO.getClaimAttachments().get(0).getClaimId());
					Claim claim = claimsDao.getClaim(claimsVO.getClaimId());
					updateClaimSummaryAmountsWithForecastAmount(claim, fileContents);
					return loadClaimAdvance(claimsVO);									
				}
				break;
			case "U":
				claimsVO.getClaimAttachments().get(0).setUpdateUser(claimsVO.getUpdateUser());
				claimsVO.getClaimAttachments().get(0).setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				claimsDao.saveOrUpdateClaimAttachment(claimsVO.getClaimAttachments().get(0));
				break;
			case "D":
				claimsDao.deleteProtocolAttachment(claimsVO.getClaimAttachments().get(0));
				break;
			case "R":
				claimsVO.getClaimAttachments().get(0).setUpdateUser(claimsVO.getUpdateUser());
				claimsVO.getClaimAttachments().get(0).setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				replaceProtocolAttachment(files,claimsVO.getClaimAttachments().get(0));
				if(claimsVO.getClaimAttachments().get(0).getTypeCode().equals("1")) {
					List<ClaimSummary> fileContents = readFileContents(claimsVO.getUpdateUser(), files[0], claimsVO.getClaimAttachments().get(0).getClaimId());
					claimsVO.setClaimId(claimsVO.getClaimAttachments().get(0).getClaimId());
					Claim claim = claimsDao.getClaim(claimsVO.getClaimAttachments().get(0).getClaimId());
					updateClaimSummaryAmountsWithForecastAmount(claim, fileContents);
					return loadClaimAdvance(claimsVO);									
				}
				break;
			default:	
				break;
			}
		} catch (Exception e) {
			logger.error("error in saveOrUpdateClaimAttachment : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(claimsVO);																																																																					 
	}

	private void updateClaimSummaryAmountsWithForecastAmount(Claim claim, List<ClaimSummary> fileContents) {
		Map<String, ClaimSummary> collect = fileContents.stream().collect(Collectors.toMap(ClaimSummary :: getBudgetCategoryCode, claimSummary -> claimSummary));
		List<ClaimSummary> claimSummary = claimsDao.loadClaimSummary(claim.getClaimId());
		BigDecimal indirectCostForecastAmount = BigDecimal.ZERO;
		BigDecimal commitmentsUptoPrevClaimAmount = BigDecimal.ZERO;
		for (ClaimSummary summary : claimSummary) {
			if (collect.containsKey(summary.getBudgetCategoryCode())) {
				BigDecimal cumAmountUntilLastClaim = summary.getCumClaimAmountUptoClaim() == null ? BigDecimal.ZERO : summary.getCumClaimAmountUptoClaim();
				BigDecimal totalExpIncuredUptoClaim = BigDecimal.ZERO;
				BigDecimal fundBalanceAtEndOfClaim = BigDecimal.ZERO;
				BigDecimal amountReqForCurrentClaim = BigDecimal.ZERO;		
				BigDecimal cumExpenseUptoPrevClaimPeriod = summary.getCumExpenseUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCumExpenseUptoPrevClaim();
				BigDecimal commitmentsUptoPrevClaim =  collect.get(summary.getBudgetCategoryCode()).getCommitmentsUptoPrevClaim() == null ?
						BigDecimal.ZERO : collect.get(summary.getBudgetCategoryCode()).getCommitmentsUptoPrevClaim();
				totalExpIncuredUptoClaim = cumExpenseUptoPrevClaimPeriod.add(summary.getAmountRequested() == null ? BigDecimal.ZERO : summary.getAmountRequested());
				fundBalanceAtEndOfClaim = cumAmountUntilLastClaim.subtract(totalExpIncuredUptoClaim);
				amountReqForCurrentClaim = commitmentsUptoPrevClaim.add(collect.get(summary.getBudgetCategoryCode())== null ?
						BigDecimal.ZERO : collect.get(summary.getBudgetCategoryCode()).getAmountForcasted()).subtract(fundBalanceAtEndOfClaim);
				summary.setAmountForcasted(collect.get(summary.getBudgetCategoryCode()).getAmountForcasted());
				summary.setAmountReqForCurrentClaim(amountReqForCurrentClaim);
				summary.setCommitmentsUptoPrevClaim(collect.get(summary.getBudgetCategoryCode()).getCommitmentsUptoPrevClaim());
				claimsDao.saveOrUpdateClaimSummary(summary);
			}
			indirectCostForecastAmount = indirectCostForecastAmount.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted());
			commitmentsUptoPrevClaimAmount = commitmentsUptoPrevClaimAmount.add(summary.getCommitmentsUptoPrevClaim() == null ? BigDecimal.ZERO :
					summary.getCommitmentsUptoPrevClaim());
		}
		BigDecimal overHeadPercentage = claim.getOverHeadPercentage() == null ? BigDecimal.ZERO : claim.getOverHeadPercentage();
		claim.setIdcAmountForcasted(indirectCostForecastAmount.multiply(overHeadPercentage.divide(new BigDecimal(100))));
		claim.setIdcCommitmentUptoPrevClaim(commitmentsUptoPrevClaimAmount.multiply(overHeadPercentage.divide(new BigDecimal(100))));
	}

	private List<ClaimSummary> readFileContents(String updateUser, MultipartFile multipartFile, Integer claimId) {
		List<ClaimSummary> claimSummaryForeCast = new ArrayList<>();
		try{
			int searchColumn = 0;
		    DataFormatter dataFormatter = new DataFormatter();
		    Workbook workbook = WorkbookFactory.create(multipartFile.getInputStream());
		    FormulaEvaluator formulaEvaluator = workbook.getCreationHelper().createFormulaEvaluator(); 
		    Sheet sheet = workbook.getSheetAt(0);
		    for (Row row : sheet) {
				if (row.getRowNum() == 0) {
					continue; //  skips the first row
				}
			   Cell cellInSearchColumn = row.getCell(searchColumn);
			   if (cellInSearchColumn != null && !dataFormatter.formatCellValue(row.getCell(0), formulaEvaluator).isEmpty() &&
					   !dataFormatter.formatCellValue(row.getCell(0), formulaEvaluator).trim().isEmpty()) {
				   ClaimSummary summary = new ClaimSummary();
				   summary.setBudgetCategoryCode(dataFormatter.formatCellValue(row.getCell(0), formulaEvaluator));
				   if (row.getCell(1) != null && !dataFormatter.formatCellValue(row.getCell(1), formulaEvaluator).isEmpty() &&
						   !dataFormatter.formatCellValue(row.getCell(1), formulaEvaluator).trim().isEmpty()) {
					   summary.setAmountForcasted(new BigDecimal(dataFormatter.formatCellValue(row.getCell(1), formulaEvaluator)));
				   }
				   if (row.getCell(2) != null && !dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator).isEmpty() &&
						   !dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator).trim().isEmpty()) {
					   summary.setCommitmentsUptoPrevClaim(new BigDecimal(dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator)));
				   }
				   claimSummaryForeCast.add(summary);
			   }
	  	    }
		    workbook.close();
		} catch (Exception e) {
			logger.error("error in readFileContents : {}", e.getMessage());
		}
		return claimSummaryForeCast;	
	}

	private void replaceProtocolAttachment(MultipartFile[] files, ClaimAttachment claimAttachment) throws IOException {
		claimAttachment.setClaimAttachmentId(null);
		claimAttachment.setVersionNumber(claimAttachment.getVersionNumber()+1);
		claimAttachment.setDocumentStatusCode("1");
		FileData fileData = new FileData();
		fileData.setAttachment(files[0].getBytes());
		fileData = commonDao.saveFileData(fileData);
		claimAttachment.setFileDataId(fileData.getFileDataId());
		claimsDao.saveOrUpdateClaimAttachment(claimAttachment);
		claimsDao.archiveOldAttachmentVersion(claimAttachment.getDocumentId(), claimAttachment.getClaimId(), claimAttachment.getUpdateUser(), claimAttachment.getVersionNumber() - 1);
	}

	private void addNewProtocolAttachment(MultipartFile[] files, List<ClaimAttachment> attachments, String updateUser) throws IOException{
		Integer documentId = claimsDao.generateDocumentId();
		int i = 0;
		while (files.length > i) {
			for (ClaimAttachment attachment : attachments) {
				documentId = documentId+1;			
				attachment.setFileName(files[i].getOriginalFilename());
				attachment.setMimeType(files[i].getContentType());
				attachment.setVersionNumber(1);
				attachment.setDocumentStatusCode("1");
				attachment.setDocumentId(documentId);
				FileData fileData = new FileData();
				fileData.setAttachment(files[i].getBytes());
				fileData = commonDao.saveFileData(fileData);
				attachment.setFileDataId(fileData.getFileDataId());
				attachment.setUpdateUser(updateUser);
				attachment.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				claimsDao.saveOrUpdateClaimAttachment(attachment);
				i++;			
			}
		}	
	}

	@Override
	public String loadClaimAttachments(ClaimsVO claimsVO) {
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_CONFIDENTIAL_CLAIM_ATTACHMENTS_RIGHT);
		Boolean isPersonHasPermission = commonDao.checkPersonHasRightInModule(Constants.AWARD_MODULE_CODE, claimsVO.getAwardId(), rightNames, AuthenticatedUser.getLoginPersonId());
		if(Boolean.FALSE.equals(isPersonHasPermission)) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_CLAIM_ATTACHMENTS_RIGHT, claimsVO.getAwardLeadUnitNumber());
		}
		List<ClaimAttachment> claimAttachments = null;
		if (claimsVO.getDocumentId() != null) {
			claimAttachments = claimsDao.loadClaimAttachmentVersions(claimsVO.getClaimId(), claimsVO.getDocumentId(), isPersonHasPermission);
		} else {
			claimAttachments = claimsDao.loadClaimAttachments(claimsVO.getClaimId(), isPersonHasPermission);
			claimsVO.setClaimAttachmentType(claimsDao.loadClaimAttachmentTypes());
		}
		getFullNameOfUpdateUser(claimAttachments);
		claimsVO.setClaimAttachments(claimAttachments);
		return commonDao.convertObjectToJSON(claimsVO);
	}
	
	private void getFullNameOfUpdateUser(List<ClaimAttachment> claimAttachments) {
		Set<String> userName = claimAttachments.stream().map(ClaimAttachment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			claimAttachments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserName(collect.get(item.getUpdateUser().toUpperCase())));
		}	
	}

	@Override
	public String updateClaimSummaryExcludeFlag(ClaimsVO claimsVO) {
		Boolean checkIfPreviouslyExcluded = claimsDao.checkIfPreviouslyExcluded(claimsVO.getClaimSummaryDetail().getClaimDetailsId());
		if (Boolean.TRUE.equals(checkIfPreviouslyExcluded)) {
			claimsVO.setClaimPreviouslyExcluded(Boolean.TRUE);
			return commonDao.convertObjectToJSON(claimsVO);
		} else {
			claimsVO.setClaimPreviouslyExcluded(Boolean.FALSE);
			if (!claimsVO.getClaimId().equals(claimsVO.getClaimSummaryDetail().getClaimId())) {
				ClaimSummaryDetails summaryDetails = claimsDao.getClaimSummaryDetailById(claimsVO.getClaimSummaryDetail().getClaimDetailsId());
				ClaimSummaryDetails newSummaryDetails = new ClaimSummaryDetails();
				BeanUtils.copyProperties(summaryDetails, newSummaryDetails);
				newSummaryDetails.setClaimSummaryId(claimsDao.getClaimSummaryIdByParams(claimsVO.getClaimId(), summaryDetails.getBudgetCategoryCode()));
				newSummaryDetails.setClaimId(claimsVO.getClaimId());
				newSummaryDetails.setClaimDetailsId(null);
				newSummaryDetails.setUpdateUser(claimsVO.getClaimSummaryDetail().getUpdateUser());
				newSummaryDetails.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				newSummaryDetails.setPrevExcludedSummaryDetId(claimsVO.getClaimSummaryDetail().getClaimDetailsId());
				newSummaryDetails.setIsExcludedFlag(false);
				claimsDao.saveOrUpdateClaimBreakDown(newSummaryDetails);
			} else if (claimsVO.getClaimSummaryDetail().getPrevExcludedSummaryDetId() != null && claimsVO.getClaimSummaryDetail().getIsExcludedFlag().equals(true)){
				claimsDao.deleteSummaryDetailForPrevClaimExcluded(claimsVO.getClaimSummaryDetail().getPrevExcludedSummaryDetId(),claimsVO.getClaimSummaryDetail().getClaimDetailsId());
			} else {
				claimsDao.updateClaimSummaryExcludeFlag(claimsVO.getClaimSummaryDetail());
			}
			BigDecimal amountRequesed = claimsDao.getClaimSummaryAmountReqForAll(claimsVO.getClaimId(), claimsVO.getClaimSummaryDetail().getBudgetCategoryCode());
			updateClaimSummaryAmountForBudgetCategory(claimsVO.getClaimId(), claimsVO.getClaimSummaryDetail().getBudgetCategoryCode(), amountRequesed);
			updateAdjustedIndirectCostFromDetailBreakDown(claimsVO.getClaimId());
			return loadClaimDetailBreakDown(claimsVO);
		}
	}

	@Override
	public String loadClaimAdvance(ClaimsVO claimsVO) {
		loadClaimAdvanceCaculation(claimsVO);		
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private void loadClaimAdvanceCaculation(ClaimsVO claimsVO) {
		List<ClaimSummary> claimSummary = claimsDao.loadClaimSummary(claimsVO.getClaimId());
		BigDecimal overHeadPercentage =  claimsDao.getOverheadPercentage(claimsVO.getClaimId());
		ClaimFundingScheme fundingScheme = null;
		BigDecimal totalInDirectOrgAprvdBudget = BigDecimal.ZERO;
		BigDecimal totalInDirectLatestAprvdBudget = BigDecimal.ZERO;
		Claim claim = new Claim();
		if (!claimSummary.isEmpty()) {
			claim = claimSummary.get(0).getClaim();
			fundingScheme = claimsDao.getClaimFundingScheme(claimSummary.get(0).getClaim().getAwardId());
			totalInDirectOrgAprvdBudget = claim.getIdcOrginalAmount() == null ? BigDecimal.ZERO : claim.getIdcOrginalAmount();
			totalInDirectLatestAprvdBudget = claim.getIdcLatestAmount() == null ? BigDecimal.ZERO : claim.getIdcLatestAmount();
		}
		ClaimFundingScheme finalFundingScheme = fundingScheme;
		claimSummary.forEach(summary -> {
			summary.setPrevClaimsTotalAmount(summary.getPrevClaimsTotalAmount() == null ? BigDecimal.ZERO : summary.getPrevClaimsTotalAmount());
			summary.setCumClaimUptoPeriod(summary.getCumClaimAmountUptoClaim() == null ? BigDecimal.ZERO : summary.getCumClaimAmountUptoClaim());		
			summary.setCumExpenseUptoPrevPeriod(summary.getCumExpenseUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCumExpenseUptoPrevClaim());
			summary.setExpenseIncuredToThisPeriod(summary.getAmountRequested() ==  null ? BigDecimal.ZERO : summary.getAmountRequested());
			summary.setComAmtUptoPeriod(summary.getCommitmentsUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCommitmentsUptoPrevClaim());
			summary.setTotalExpenseIncuredUptoPeriod(summary.getExpenseIncuredToThisPeriod().add(summary.getCumExpenseUptoPrevPeriod() == null ? BigDecimal.ZERO : summary.getCumExpenseUptoPrevPeriod()));
			summary.setBalanceAmtTillPrevPeriod(summary.getCumClaimUptoPeriod().subtract(summary.getTotalExpenseIncuredUptoPeriod()));
			summary.setAmountForcasted(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted());
			summary.setAmtReqForCurrentPeroid(summary.getAmountReqForCurrentClaim() == null ? BigDecimal.ZERO : summary.getAmountReqForCurrentClaim());
			if(finalFundingScheme != null && finalFundingScheme.getOverrideNegativeAmount())
				summary.setAmtReqForCurrentPeroid(new BigDecimal(Math.max(Double.valueOf(summary.getAmtReqForCurrentPeroid().toString()),0)));
		});
		Map<String, Object> advanceCalculationsParent = new HashMap<>();
		Map<String, Object> advanceCalculations = new HashMap<>();
		BigDecimal totalOrgAprvdBudget = claimSummary.parallelStream().filter(x -> x.getOriginalApprovedBudget() != null)
				.map(ClaimSummary::getOriginalApprovedBudget).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalLatestAprvdBudget = claimSummary.parallelStream().filter(x -> x.getLatestApprovedBudget() != null)
				.map(ClaimSummary::getLatestApprovedBudget).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalCumClaimUptoPeriod = claimSummary.parallelStream().filter(x -> x.getCumClaimUptoPeriod() != null)
				.map(ClaimSummary::getCumClaimUptoPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalCumExpUptoPeriod = claimSummary.parallelStream().filter(x -> x.getCumExpenseUptoPrevPeriod() != null)
				.map(ClaimSummary::getCumExpenseUptoPrevPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalExpIncToThisPeriod =  claimSummary.parallelStream().filter(x -> x.getExpenseIncuredToThisPeriod() != null)
				.map(ClaimSummary::getExpenseIncuredToThisPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalExpIncUpToPeriod = claimSummary.parallelStream().filter(x -> x.getTotalExpenseIncuredUptoPeriod() != null)
				.map(ClaimSummary::getTotalExpenseIncuredUptoPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalbalanceAmtTillPrevPeriod = claimSummary.parallelStream().filter(x -> x.getBalanceAmtTillPrevPeriod() != null)
				.map(ClaimSummary::getBalanceAmtTillPrevPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalcomAmtUptoPeriod = claimSummary.parallelStream().filter(x -> x.getComAmtUptoPeriod() != null)
		.		map(ClaimSummary::getComAmtUptoPeriod).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalForeCastAmount = claimSummary.parallelStream().filter(x -> x.getAmountForcasted() != null)
				.map(ClaimSummary::getAmountForcasted).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal totalAmountReq = claimSummary.parallelStream().filter(x -> x.getAmtReqForCurrentPeroid() != null)
				.map(ClaimSummary::getAmtReqForCurrentPeroid).reduce(BigDecimal.ZERO,BigDecimal::add);
		if(finalFundingScheme != null && finalFundingScheme.getOverrideNegativeAmount())
			totalAmountReq = new BigDecimal(Math.max(Double.valueOf(totalAmountReq.toString()),0));
		advanceCalculations.put("totalOrgAprvdBudget", totalOrgAprvdBudget);
		advanceCalculations.put("totalLatestAprvdBudget", totalLatestAprvdBudget);
		advanceCalculations.put("totalCumClaimUptoPeriod", totalCumClaimUptoPeriod);
		advanceCalculations.put("totalCumExpUptoPeriod", totalCumExpUptoPeriod);
		advanceCalculations.put("totalExpIncToThisPeriod", totalExpIncToThisPeriod);
		advanceCalculations.put("totalExpIncUpToPeriod", totalExpIncUpToPeriod);
		advanceCalculations.put("totalbalanceAmtTillPrevPeriod", totalbalanceAmtTillPrevPeriod);
		advanceCalculations.put("totalcomAmtUptoPeriod", totalcomAmtUptoPeriod);
		advanceCalculations.put("totalForeCastAmount", totalForeCastAmount);
		advanceCalculations.put("totalAmountReq", totalAmountReq);
		advanceCalculationsParent.putAll(new HashMap<>(advanceCalculations));
		advanceCalculations.clear();
		advanceCalculations.put("totalInDirectOrgAprvdBudget", totalInDirectOrgAprvdBudget);
		advanceCalculations.put("totalInDirectLatestAprvdBudget", totalInDirectLatestAprvdBudget);
		BigDecimal totalInDirectCumClaimUptoPeriod = claim.getIdcCumClaimAmtUptoClaim() == null ? BigDecimal.ZERO : claim.getIdcCumClaimAmtUptoClaim();  	
		advanceCalculations.put("totalInDirectCumClaimUptoPeriod", totalInDirectCumClaimUptoPeriod);
		BigDecimal totalInDirectExpIncUpToPeriod = totalExpIncUpToPeriod.multiply(overHeadPercentage.divide(new BigDecimal(100)));
		advanceCalculations.put("totalInDirectExpIncUpToPeriod", totalInDirectExpIncUpToPeriod);
		BigDecimal totalInDirectbalanceAmtTillPrevPeriod = totalInDirectCumClaimUptoPeriod.subtract(totalInDirectExpIncUpToPeriod);
		advanceCalculations.put("totalInDirectbalanceAmtTillPrevPeriod", totalInDirectbalanceAmtTillPrevPeriod);
		BigDecimal totalInDirectcomAmtUptoPeriod = claim.getIdcCommitmentUptoPrevClaim() == null ? BigDecimal.ZERO : claim.getIdcCommitmentUptoPrevClaim();
		advanceCalculations.put("totalInDirectcomAmtUptoPeriod", totalInDirectcomAmtUptoPeriod);
		BigDecimal totalInDirectForeCastAmount = claim.getIdcAmountForcasted() == null ? BigDecimal.ZERO : claim.getIdcAmountForcasted();
		advanceCalculations.put("totalInDirectForeCastAmount", totalInDirectForeCastAmount);
		BigDecimal totalInDirectAmountReq = totalInDirectcomAmtUptoPeriod.add(totalInDirectForeCastAmount).subtract(totalInDirectbalanceAmtTillPrevPeriod);
		if(finalFundingScheme != null && finalFundingScheme.getOverrideNegativeAmount())
			totalInDirectAmountReq = new BigDecimal(Math.max(Double.valueOf(totalInDirectAmountReq.toString()),0));
		advanceCalculations.put("totalInDirectAmountReq", totalInDirectAmountReq);
		advanceCalculations.put("grantTotalOrgAprvdBudget", totalOrgAprvdBudget.add(totalInDirectOrgAprvdBudget));
		advanceCalculations.put("grantTotalLatestAprvdBudget", totalLatestAprvdBudget.add(totalInDirectLatestAprvdBudget));
		advanceCalculations.put("grantTotalCumClaimUptoPeriod", totalCumClaimUptoPeriod.add(totalCumClaimUptoPeriod.multiply(overHeadPercentage.divide(new BigDecimal(100)))));
		advanceCalculations.put("grantTotalCumExpUptoPeriod", totalCumExpUptoPeriod);
		advanceCalculations.put("grantTotalExpIncToThisPeriod", totalExpIncToThisPeriod);
		advanceCalculations.put("grantTotalExpIncUpToPeriod", totalExpIncUpToPeriod.add(totalExpIncUpToPeriod.multiply(overHeadPercentage.divide(new BigDecimal(100)))));
		advanceCalculations.put("grantTotalbalanceAmtTillPrevPeriod", totalbalanceAmtTillPrevPeriod.add(totalbalanceAmtTillPrevPeriod.multiply(overHeadPercentage.divide(new BigDecimal(100)))));
		advanceCalculations.put("grantTotalcomAmtUptoPeriod", totalcomAmtUptoPeriod.add(totalcomAmtUptoPeriod.multiply(overHeadPercentage.divide(new BigDecimal(100)))));
		advanceCalculations.put("grantTotalForeCastAmount", totalForeCastAmount.add(totalForeCastAmount.multiply(overHeadPercentage.divide(new BigDecimal(100)))));
		advanceCalculations.put("grantTotalAmountReq", totalAmountReq.add(totalInDirectAmountReq));
		advanceCalculationsParent.putAll(new HashMap<>(advanceCalculations));
		advanceCalculations.clear();
		advanceCalculations.put("totalGrantRequired", totalAmountReq.add(totalInDirectAmountReq));
		advanceCalculationsParent.putAll(new HashMap<>(advanceCalculations));
		claimsVO.setAdvanceCalculations(advanceCalculationsParent);
		claimsVO.setClaimSummary(claimSummary);
		claimsVO.setClaimAttachment(claimsDao.getClaimForeCastAttachmentDetail(claimsVO.getClaimId()));		
	}

	@Override
	public ResponseEntity<byte[]> downloadClaimAttachment(Integer attachmentId) {
		ClaimAttachment claimAttachment = claimsDao.getClaimAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(claimAttachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(claimAttachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("Exception in downloadClaimAttachment {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String loadClaimManpower(ClaimsVO claimsVO) {
		Claim claim = claimsVO.getClaim();
		List<AwardManpowerResource> awardManpowerResources = claimsDao.loadAwardManpowerResource(claim.getAwardNumber(), claim.getSequenceNumber());
		awardManpowerResources.stream().filter(manpowerResource -> manpowerResource.getChargeStartDate() != null && manpowerResource.getChargeEndDate() != null).forEach(manpowerResource -> {
			manpowerResource.setInvolvementFrom((claim.getStartDate().compareTo(manpowerResource.getChargeStartDate()) > 0  ? new Timestamp(claim.getStartDate().getTime()) : manpowerResource.getChargeStartDate()));		
			manpowerResource.setInvolvementTo((claim.getEndDate().compareTo(manpowerResource.getChargeEndDate()) > 0  ? manpowerResource.getChargeEndDate() : new Timestamp(claim.getEndDate().getTime())));
			setInvolvementFromAndToDate(manpowerResource);
			if (manpowerResource.getChargeStartDate().after(claim.getEndDate()) || manpowerResource.getChargeEndDate().before(claim.getStartDate())) {
				manpowerResource.setInvolvementFrom(null);
				manpowerResource.setInvolvementTo(null);				
			}
		});
		Set<Integer> awardmanpowerIds = awardManpowerResources.stream().map(AwardManpowerResource::getAwardManpowerId).collect(Collectors.toSet());
		if (!awardmanpowerIds.isEmpty()) {
			List<AwardManpower> awardmanpowerResources = claimsDao.getAllAwardManpower(awardmanpowerIds);
			Map<Integer, List<AwardManpower>> collect = awardmanpowerResources.stream().collect(Collectors.groupingBy(AwardManpower::getAwardManpowerId));
			awardManpowerResources.stream().filter(item -> collect.containsKey(item.getAwardManpowerId())).forEach(item -> item.setAwardManpower(collect.get(item.getAwardManpowerId()).get(0)));
		} 
		Set<String> personIds = awardManpowerResources.stream().map(AwardManpowerResource::getPersonId).collect(Collectors.toSet());
		if (!personIds.isEmpty()) {
			List<Manpower> awardmanpowerResources = claimsDao.getAllManpower(personIds);
			Map<String, List<Manpower>> collect = awardmanpowerResources.stream().collect(Collectors.groupingBy(Manpower::getManpowerPersonId));
			awardManpowerResources.stream().filter(item -> collect.containsKey(item.getPersonId())).forEach(item -> {
				Manpower manpower = collect.get(item.getPersonId()).get(0);
				try {
					manpower.setDecryptedCitizenShip(manpower.getCitizenship() != null ? excelityService.decryptAESData(manpower.getCitizenship()) : null);
					manpower.setDecryptedNationality(manpower.getNationality() != null ? excelityService.decryptAESData(manpower.getNationality()) : null);
				} catch (Exception e) {
					logger.error("error while decrytping data", e);
				}
				item.setManpower(manpower);
			});
		}
		Set<Integer> manpowerResourceIds = awardManpowerResources.stream().map(AwardManpowerResource::getManpowerResourceId).collect(Collectors.toSet());
		if (!manpowerResourceIds.isEmpty()) {
			List<ClaimManpower> claimManpowers = claimsDao.getAllClaimManpower(manpowerResourceIds);
			Map<Integer, List<ClaimManpower>> collect = claimManpowers.stream().collect(Collectors.groupingBy(ClaimManpower::getAwardManpowerResourceId));
			awardManpowerResources.stream().filter(item -> collect.containsKey(item.getManpowerResourceId())).forEach(item -> {
			ClaimManpower claimManpower = collect.get(item.getManpowerResourceId()).get(0);
			if (claimManpower.getIsJobReqPHDQualification() != null) {
				claimManpower.setJobReqPHDQualificationValue(Boolean.TRUE.equals(claimManpower.getIsJobReqPHDQualification()) ? "YES" : "NO");
			} else {
				claimManpower.setJobReqPHDQualificationValue("");
			}
			if (claimManpower.getIsGrantUsed() != null) {
				claimManpower.setGrantUsedValue(Boolean.TRUE.equals(claimManpower.getIsGrantUsed()) ? "YES" : "NO");
			} else {
				claimManpower.setGrantUsedValue("");
			}
			if (claimManpower.getIsApprovedForForeignStaff() != null) {
				claimManpower.setApprovedForForeignStaff(Boolean.TRUE.equals(claimManpower.getIsApprovedForForeignStaff()) ? "YES" : "NO");
			} else {
				claimManpower.setApprovedForForeignStaff("");
			}
			if (claimManpower.getUpdateUser()!= null) {
				claimManpower.setFullName(personDao.getUserFullNameByUserName(claimManpower.getUpdateUser()));				
			}
			item.setClaimManpower(claimManpower);
			});
		}
		getUserDetails(awardManpowerResources);
		List<AwardManpowerResource> currentManPowerResource = new ArrayList<>();
		List<AwardManpowerResource> pastManpowerResource = new ArrayList<>();		
		awardManpowerResources.stream().filter(awardManpowerResource-> (awardManpowerResource.getAwardManpower().getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF) || 
				awardManpowerResource.getAwardManpower().getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STUDENT)) && awardManpowerResource.getPersonId() != null)
		.forEach(awardManpowerResource -> {
			if (awardManpowerResource.getChargeEndDate() != null && awardManpowerResource.getChargeEndDate().before(claim.getStartDate())) {
				pastManpowerResource.add(awardManpowerResource);
			} else if (awardManpowerResource.getChargeStartDate() != null && !awardManpowerResource.getChargeStartDate().after(claim.getEndDate())) {
				currentManPowerResource.add(awardManpowerResource);
			}
		});		
		claimsVO.setCurrentManPowerResource(currentManPowerResource);
		claimsVO.setCurrentHeadCount(currentManPowerResource.size());
		claimsVO.setPastManpowerResource(pastManpowerResource);
		claimsVO.setAllAwardManpowerResource(awardManpowerResources);
		Set<Integer> manpowerIds = currentManPowerResource.stream().map(AwardManpowerResource::getAwardManpowerId).collect(Collectors.toSet());
		Set<Integer> manpowerResourceId = currentManPowerResource.stream().map(AwardManpowerResource::getManpowerResourceId).collect(Collectors.toSet());
		if(!manpowerResourceId.isEmpty()) {
			List<Object[]> actualManpowerCounts = claimsDao.getActualCountOfManpowerResourceBasedOnJobProfile(manpowerResourceId);
			claimsVO.setActualManpowerCounts(prepareManpowerCounts(actualManpowerCounts));
		}
		if (!manpowerIds.isEmpty()) {
			List<Object[]> approvedManpowerCounts = claimsDao.getApprovedCountOfManpowerResourceBasedOnJobProfile(manpowerIds);
			claimsVO.setApprovedManpowerCounts(prepareManpowerCounts(approvedManpowerCounts));
		}
		claimsVO.setClaim(null);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private AwardManpowerResource setInvolvementFromAndToDate(AwardManpowerResource resource) {
		if (resource.getInvolvementFrom() != null && resource.getInvolvementTo() != null) {
			String involvementFrom = new SimpleDateFormat(Constants.REQUIRED_DATE_FORMAT).format(new Date(resource.getInvolvementFrom().getTime()));
			String involvementTo = new SimpleDateFormat(Constants.REQUIRED_DATE_FORMAT).format(new Date(resource.getInvolvementTo().getTime()));
			resource.setInvolvementPeriod(involvementFrom + " to " + involvementTo);
		}
		return resource;
	}

	private List<AwardManpowerResource> prepareManpowerCounts(List<Object[]> jobProfileBasedManpowerCounts) {
		List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
		Integer othersCount = 0;
		for(Object[] manPower : jobProfileBasedManpowerCounts) {
			AwardManpowerResource manpowerResource = new AwardManpowerResource();
			Object jobTitle = manPower[0];
			Object count = manPower[1];
			manpowerResource.setJobProfileTitle(jobTitle != null ? jobTitle.toString() : "");
			if (count != null) {
				manpowerResource.setManpowerCount(Integer.parseInt(count.toString().split("\\.")[0]));
				if (!manpowerResource.getJobProfileTitle().isEmpty()) {
					othersCount = othersCount + Integer.parseInt(count.toString().split("\\.")[0]);
				} 
			} else {
				manpowerResource.setManpowerCount(0);
			}
			awardManpowerResources.add(manpowerResource);
		}
//		AwardManpowerResource resourceCount = new AwardManpowerResource();
//		resourceCount.setJobProfileTitle("Others");		
//		resourceCount.setManpowerCount(othersCount);
//		awardManpowerResources.add(resourceCount);
		return awardManpowerResources;
	}

	private void getUserDetails(List<AwardManpowerResource> awardManpowerResource) {
		Set<String> personId = awardManpowerResource.stream().map(AwardManpowerResource::getPersonId).collect(Collectors.toSet());
		if (!personId.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(personId));
			Map<String, List<Person>> persons = personDetails.stream().collect(Collectors.groupingBy(Person::getPersonId));
			awardManpowerResource.stream().filter(manpowerResource -> persons.containsKey(manpowerResource.getPersonId())).forEach(manpowerResource -> {
				if (persons.get(manpowerResource.getPersonId()).get(0).getUnit() != null)
					manpowerResource.setDepartment(persons.get(manpowerResource.getPersonId()).get(0).getUnit().getUnitName());
				manpowerResource.setPersonStatus((persons.get(manpowerResource.getPersonId()).get(0).getStatus()).equals(Constants.ACTIVE_PERSON) ? "Employed" : "Resigned");
				manpowerResource.setDateOfBirth(persons.get(manpowerResource.getPersonId()).get(0).getDateOfBirth());
			});
		}
		Set<Integer> rolodexId = awardManpowerResource.stream().map(AwardManpowerResource::getRolodexId).collect(Collectors.toSet());
		if (!rolodexId.isEmpty()) {
			List<Rolodex> rolodexDetails = commonDao.getRolodexDetailByRolodexId(new ArrayList<>(rolodexId));
			Map<Integer, List<Rolodex>> rolodex = rolodexDetails.stream().collect(Collectors.groupingBy(Rolodex::getRolodexId));
			awardManpowerResource.stream().filter(manpowerResource -> rolodex.containsKey(manpowerResource.getRolodexId())).forEach(manpowerResource -> {
				if (rolodex.get(manpowerResource.getRolodexId()).get(0).getOwnedByUnit() != null)
					manpowerResource.setDepartment(commonDao.getUnitByUnitNumber(rolodex.get(manpowerResource.getRolodexId()).get(0).getOwnedByUnit()).getUnitName());
				manpowerResource.setPersonStatus(null);
			});
		}
	}

	@Override
	public ResponseEntity<byte[]> generateClaimReport(ClaimsVO vo, HttpServletResponse response) {
		Claim claim = claimsDao.getClaim(vo.getClaimId());
		vo.setClaim(claim);
		vo.setLeadUnitNumber(claim.getAward().getLeadUnitNumber());
		String fileName = claim.getClaimNumber() +"_Claim_Report";
		List<Object> templateTypeCodes = claimsDao.getLetterTemplateTypeCodeToExportClaim(claim.getAwardId());
		setClaimDetails(vo, claim);
		prepareZipFile(vo, response, fileName, templateTypeCodes);
		return null;
	}

	private void setClaimDetails(ClaimsVO vo, Claim claim) {
		String claimStartDate = new SimpleDateFormat(Constants.EXCEL_DATE_FORMAT).format(new Date(claim.getStartDate().getTime()));
		String claimEndDate = new SimpleDateFormat(Constants.EXCEL_DATE_FORMAT).format(new Date(claim.getEndDate().getTime()));
		vo.setClaimStartAndEndYear(claimStartDate + " to " + claimEndDate);
		vo.setClaimQuarterFrom(findClaimQuarter(claim.getStartDate()));
		vo.setClaimQuarterTo(findClaimQuarter(claim.getEndDate()));
		vo.setClaimHalfYearlyPeriod(findHalfYearlyPeriod(claim.getStartDate()));
		String claimPeriodStartDate = new SimpleDateFormat(Constants.PROGRESS_REPORT_DATE_FORMAT).format(new Date(claim.getStartDate().getTime()));
		String claimPeriodEndDate = new SimpleDateFormat(Constants.PROGRESS_REPORT_DATE_FORMAT).format(new Date(claim.getEndDate().getTime()));
		vo.setClaimPeriodStartAndEndYear(claimPeriodStartDate + " to " + claimPeriodEndDate);
		String awardStartDate =  new SimpleDateFormat(Constants.EXCEL_LONG_DATE_FORMAT).format(new Date(claim.getAward().getBeginDate().getTime()));
		String awardEndDate =  new SimpleDateFormat(Constants.EXCEL_LONG_DATE_FORMAT).format(new Date(claim.getAward().getFinalExpirationDate().getTime()));
		vo.setLastClaimEndDate(claimsDao.getLastClaimEndDate(claim.getAwardNumber(), vo.getClaimId()));
		if (vo.getLastClaimEndDate() != null) {
			vo.setPreviousClaimEndDate(String.valueOf(new SimpleDateFormat(Constants.EXCEL_DATE_FORMAT).format(new Date(vo.getLastClaimEndDate().getTime()))));
		}
		vo.setAwardStartDate(awardStartDate);
		vo.setAwardEndDate(awardEndDate);
		vo.setProjectDuration(awardStartDate + " to " + awardEndDate);
		vo.setFinancialYear("FY" + claim.getClaimNumber().substring(1, 5));
	}

	private String findHalfYearlyPeriod(Date startDate) {
		Calendar c = Calendar.getInstance();
		c.setTime(startDate);
		int month = c.get(Calendar.MONTH);
		return 	(month >= Calendar.APRIL && month <= Calendar.SEPTEMBER) ? "H1" : "H2";
	}

	private String findClaimQuarter(Date startDate) {
		Calendar c = Calendar.getInstance();
		c.setTime(startDate);
		int month = c.get(Calendar.MONTH);
		return (month >= Calendar.JANUARY && month <= Calendar.MARCH) ? "4"
				: (month >= Calendar.APRIL && month <= Calendar.JUNE) ? "1"
				: (month >= Calendar.JULY && month <= Calendar.SEPTEMBER) ? "2"
				: "3";
	}

	private void prepareManpowerSheet(ClaimsVO vo) {
		Claim claim = vo.getClaim();
		claim.setAwardStartDate(claim.getAward().getBeginDate());
		claim.setAwardEndDate(claim.getAward().getFinalExpirationDate());
		loadClaimManpower(vo);
		vo.setClaim(claim);
		if (vo.getAllAwardManpowerResource().isEmpty()) {
			vo.getAllAwardManpowerResource().add(new AwardManpowerResource());
		}
	}

	private void prepareDetailBreakDownSheet(ClaimsVO vo) {
		vo.setIsExcelExport(Boolean.TRUE);
		loadClaimDetailBreakDown(vo);
		if (vo.getClaimSummaryDetails() != null && vo.getClaimSummaryDetails().isEmpty()) {
			vo.getClaimSummaryDetails().add(new LinkedHashMap<>());
		}
	}

	private byte[] prepareClaimExcelSheet( ClaimsVO vo, String templateTypeCode, Claim claim) {
		byte[] bFile = printService.getTemplateData(templateTypeCode);
		InputStream inputStream = new ByteArrayInputStream(bFile);
		 Context context = new Context();
		try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()){ 
			context.putVar(CLAIMVO, vo);
			logger.info("Applying at cell Details for Claim_General_Report");
			JxlsHelper.getInstance().processTemplate(inputStream, outputStream, context);
            logger.info("written to default template file");
        	return outputStream.toByteArray();
        } catch (Exception e) {
			logger.error("Exception in prepareClaimExcelSheet : {}", e.getMessage());
			return new byte[0];
		}
	}

	private void prepareMainSheet(ClaimsVO vo) {
		Claim claim = vo.getClaim();
		vo.setClaim(claim);
		SimpleDateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
		if (claim.getClaimSubmissionDate() != null) {
			vo.setClaimSubmissionDate(String.valueOf(dateFormat.format(new Date(claim.getClaimSubmissionDate().getTime()))));
		}
		if (claim.getFoApprovalDate() != null) {
			vo.setClaimFOApprovalDate(String.valueOf(dateFormat.format(new Date(claim.getFoApprovalDate().getTime()))));
		}
		if (claim.getRsoApprovalDate() != null) {
			vo.setClaimRSOApprovalDate(String.valueOf(dateFormat.format(new Date(claim.getRsoApprovalDate().getTime()))));
		}
		if (claim.getRdoApprovalDate() != null) {
			vo.setClaimRDOApprovalDate(String.valueOf(dateFormat.format(new Date(claim.getRdoApprovalDate().getTime()))));
		}
		loadClaimReimbursement(vo);
	}

	@Override
	public String loadSubmittedClaimsForAward(ClaimsVO claimsVO) {
		List<Object[]> claimsObject = claimsDao.loadClaimsForAward(claimsVO.getAwardNumber());
		List<Claim> claims = new ArrayList<>();
		claimsObject.forEach(claimObject -> {
			Claim claim = new Claim();
			claim.setClaimId(Integer.parseInt(claimObject[0].toString()));
			claim.setClaimNumber(claimObject[1].toString());
			claim.setAwardId(Integer.parseInt(claimObject[2].toString()));
			claim.setAwardNumber(claimObject[3].toString());
			try {
				Date startDate = new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[4].toString());  
				Date endDate = new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[5].toString());  
				Date submissionDate = claimObject[6] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[6].toString()) : null; 
				Date documentDate = claimObject[11] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[11].toString()) : null; 
				Date paymentDate = claimObject[12] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[12].toString()) : null; 
				claim.setStartDate(startDate);
				claim.setEndDate(endDate);
				claim.setClaimSubmissionDate(submissionDate);
				claim.setDocumentDate(documentDate);
				claim.setPaymentDate(paymentDate);
			} catch (Exception e) {
				logger.error("error occured while parsing date in award claims", e);
			}			 			
			ClaimStatus claimStatus = new ClaimStatus();
			claimStatus.setClaimStatusCode(claimObject[7].toString());
			claimStatus.setDescription(claimObject[8].toString());
			claim.setClaimStatus(claimStatus);
			claim.setTotalAmount(new BigDecimal(claimObject[9].toString()));
			claim.setOutputDocNumber(claimObject[10] != null ? claimObject[10].toString() : null);
			claims.add(claim);
		});
		claimsVO.setClaims(claims);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public String performClaimFOActions(ClaimsVO claimsVO) {
		switch (claimsVO.getActionType()) {
		case Constants.SUBMIT_FOR_FA_REVIEW:
			claimsDao.updateClaimDetailByParams(claimsVO.getClaimId(),Constants.CLAIM_STATUS_CODE_SUBMITTED_FOR_FA, null, null, null);
			claimsVO.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_SUBMITTED_FOR_FA));
			claimsVO.setStatus(true);
			break;
		case Constants.REVISION_REQ_FA_REVIEW:
			if(claimsVO.getClaimStatusCode() != null && (claimsVO.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_INVOICE_GENERATED) ||
					claimsVO.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_INVOICE_NOT_GENERATED)) && claimsDao.canPerformRevisionClaim(claimsVO.getClaimId())) {
				claimsDao.updateClaimDetailByParams(claimsVO.getClaimId(), Constants.CLAIM_STATUS_CODE_REVISION_BY_FA, null, null, null);
				claimsVO.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_REVISION_BY_FA));
				createNewClaimInvoiceVersion(claimsVO.getClaimId());
				claimsVO.setStatus(true);
			} else if(claimsVO.getClaimStatusCode() != null && !claimsVO.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_INVOICE_GENERATED) &&
					!claimsVO.getClaimStatusCode().equals(Constants.CLAIM_STATUS_CODE_INVOICE_NOT_GENERATED)){
				claimsDao.updateClaimDetailByParams(claimsVO.getClaimId(), Constants.CLAIM_STATUS_CODE_REVISION_BY_FA, null, null, null);
				claimsVO.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_REVISION_BY_FA));
				claimsVO.setStatus(true);
			}
			break;
		case Constants.APPROVED_BY_FA_REVIEW:
			String claimStatus = "";
			List<ClaimInvoiceDetails> claimInvoiceDetails = claimsDao.getCliamInvoiceDetailByClaimId(claimsVO.getClaimId());
			Boolean claimInvoiceDetailExist =  !claimInvoiceDetails.isEmpty() ? Boolean.TRUE :Boolean.FALSE;
			if (Boolean.FALSE.equals(claimInvoiceDetailExist)) {
				claimStatus = Constants.CLAIM_STATUS_CODE_COMPLETED;
			} else  {
				claimStatus = Constants.CLAIM_STATUS_CODE_APPROVED_BY_FA;
			}
			claimsDao.updateClaimDetailByParams(claimsVO.getClaimId(), claimStatus, null, null, claimsVO.getFunderApprovalDate());
			claimsVO.setClaimStatus(claimsDao.getClaimStatusByStatusCode(claimStatus));
			claimsVO.setStatus(true);
			break;
		case Constants.TRIGGER_INVOICE:
			claimsDao.updateClaimDetailByParams(claimsVO.getClaimId(), Constants.CLAIM_STATUS_CODE_INVOICE_TRIGGERED, null, null, claimsVO.getFunderApprovalDate());
			insertIntoSapFeed(claimsVO.getClaimId());
			claimsVO.setClaimStatus(claimsDao.getClaimStatusByStatusCode(Constants.CLAIM_STATUS_CODE_INVOICE_TRIGGERED));
			claimsVO.setStatus(true);
			break;
		default:
			break;
		}
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private void insertIntoSapFeed(Integer claimId) {
		Claim claim = claimsDao.getClaim(claimId);
		ClaimInvoice invoice = claimsDao.loadClaimInvoice(claimId);
		SapClaimFeed sapClaimFeed = new SapClaimFeed();
		sapClaimFeed.setClaimId(claimId);
		sapClaimFeed.setClaimNumber(claim.getClaimNumber());
		sapClaimFeed.setInvoiceId(invoice.getInvoiceId());
		sapClaimFeed.setSequenceNumber(invoice.getSequenceNumber());
		sapClaimFeed.setFeedType(Constants.SAP_FEED_TYPE_NEW);
		sapClaimFeed.setFeedStatus(Constants.SAP_FEED_STATUS_PENDING);
		sapClaimFeed.setUserActionCode(Constants.SAP_FEED_USER_ACTION_QUEUE_TO_FEED);
		sapClaimFeed.setBusinessArea(invoice.getCompanyCode());
		sapClaimFeed.setNoFeedFlag(false);
		sapClaimFeed.setUpdateUser(AuthenticatedUser.getLoginUserName());
		claimsDao.saveOrUpdateSapClaimFeed(sapClaimFeed);
	}

	@Override
	public String saveOrUpdateClaimManpower(ClaimsVO claimsVO) {
		claimsVO.getClaimManpower().setUpdateUser(AuthenticatedUser.getLoginUserName());
		claimsVO.setClaimManpower(claimsDao.saveOrUpdateClaimManpower(claimsVO.getClaimManpower()));
		claimsVO.getClaimManpower().setFullName(personDao.getUserFullNameByUserName(claimsVO.getClaimManpower().getUpdateUser()));
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public ResponseEntity<byte[]> downloadClaimForcastTemplate() {
		return printService.setAttachmentContent("Claim forecast template", printService.getTemplateData(Constants.CLAIM_FORECAST_TEMPLATE));
	}

	@Override
	public String updateAdjustedIndirectCost(ClaimsVO claimsVO) {
		claimsDao.updateAdjustedIndirectCost(claimsVO.getAdjustedIndirectCost(), claimsVO.getClaimId());
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public String evaluateClaimIndirectCost(Integer claimId) {
		List<ClaimSummaryDetails> claimSummaryDetails = claimsDao.loadClaimDetailBreakDown(claimId);
		claimSummaryDetails.stream().filter(summary -> summary.getBudgetCategoryCode().equals("EOM") || summary.getBudgetCategoryCode().equals("RSS")).filter(summary -> summary.getTotalAmount() == null).forEach( summary -> {
			try {
				summary.setTotalAmount(summary.getEncryptedAmount() != null ? new BigDecimal(excelityService.decryptAESData(summary.getEncryptedAmount())) : summary.getTotalAmount());
			} catch (Exception e) {
				logger.error("Exception in evaluateClaimIndirectCost - decryptAESData : {}", e.getMessage());
			}
		});
		BigDecimal totalAmount =  claimSummaryDetails.parallelStream().filter(x -> x.getTotalAmount() != null &&
				(x.getPrevAdjustedSummaryDetId() == null || x.getPrevExcludedSummaryDetId() != null))
				.map(ClaimSummaryDetails::getTotalAmount).reduce(BigDecimal.ZERO,BigDecimal::add);
		BigDecimal overHead = claimsDao.getOverheadPercentage(claimId);
		BigDecimal inDirectTotalAmount = totalAmount.multiply(overHead.divide(new BigDecimal(100))).setScale(2, RoundingMode.HALF_UP);
		BigDecimal inDirectAdjustedAmount = claimsDao.getAdjustedIndirectCost(claimId);
		List<Object> output = new ArrayList<>();
		if(inDirectTotalAmount.compareTo(inDirectAdjustedAmount) != 0){
			Map<String, String> result = new HashMap<>();
			result.put("validationMessage", "Overheads/Indirect Cost has been amended, please confirm to proceed.");
			result.put("validationType", "VW");
			output.add(result);
		}
		return commonDao.convertObjectToJSON(output);
	}

	private List<AwardExpenseTransactionVO> prepareAwardExpenseTransactions(String awardNumber, String accountNumber, Timestamp startDate, Timestamp endDate,
			String actualOrCommittedFlag, String templateTypeCode, String sponsorAwardNumber, Integer claimId) {
		if (actualOrCommittedFlag != null && actualOrCommittedFlag.equals("C")) {
			return prepareExpenseForCommittedTransaction(awardNumber, accountNumber, startDate, endDate,
					 actualOrCommittedFlag, templateTypeCode, sponsorAwardNumber, claimId);
		} else {
			return prepareExpenseForActualTransaction(awardNumber, accountNumber, startDate, endDate,
					 actualOrCommittedFlag, templateTypeCode, sponsorAwardNumber, claimId);
		}		
	}

	private List<AwardExpenseTransactionVO> prepareExpenseForActualTransaction(String awardNumber, String accountNumber, Timestamp startDate,
			Timestamp endDate, String actualOrCommittedFlag, String templateTypeCode, String sponsorAwardNumber,
			Integer claimId) {
		List<AwardExpenseTransactionVO> awardExpenseTransactionVOs = new ArrayList<>();
		List<ClaimSummaryDetails> summaryDetails = claimsDao.loadClaimDetailBreakDown(claimId);
		SimpleDateFormat dFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
		Map<String, String> budgetCategoryDetail= new HashMap<>();
		SimpleDateFormat dateFormat = new SimpleDateFormat(templateTypeCode.equals(Constants.CLAIM_IGMS_TEMPLATE_TYPE) ? "dd-MMM-yy" : "dd.MM.yyyy");
		summaryDetails.stream().sorted(Comparator.comparing(ClaimSummaryDetails::getBudgetCategoryCode)).filter(summaryDetail -> summaryDetail.getIsExcludedFlag().equals(false)).forEach(summaryDetail -> {		
			AwardExpenseTransaction awardExpenseTransaction = summaryDetail.getAwardExpenseTransaction();
			AwardExpenseTransactionVO awardExpenseTransactionVO = new AwardExpenseTransactionVO();
			awardExpenseTransactionVO.setAccountNumber(awardExpenseTransaction.getAccountNumber() != null ? awardExpenseTransaction.getAccountNumber() : "");
			awardExpenseTransactionVO.setAwardNumber(awardExpenseTransaction.getAwardNumber() != null ? awardExpenseTransaction.getAwardNumber() : "");
			awardExpenseTransactionVO.setSponsorAwardNumber(sponsorAwardNumber != null ? sponsorAwardNumber + "-01" : "");
			awardExpenseTransactionVO.setRemarks(awardExpenseTransaction.getRemarks() != null ? awardExpenseTransaction.getRemarks() : "");
			awardExpenseTransactionVO.setDocumentNumber(awardExpenseTransaction.getDocumentNumber() != null ? awardExpenseTransaction.getDocumentNumber() : "");
			awardExpenseTransactionVO.setPoNumber(awardExpenseTransaction.getPoNumber() != null ? awardExpenseTransaction.getPoNumber() : "");
			awardExpenseTransactionVO.setFmPostingDate(awardExpenseTransaction.getFmPostingDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getFmPostingDate().getTime()))) : "");
			awardExpenseTransactionVO.setDocumentDate(awardExpenseTransaction.getDocumentDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getDocumentDate().getTime()))) : "");
			awardExpenseTransactionVO.setInvoiceDate(awardExpenseTransaction.getInvoiceDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getInvoiceDate().getTime()))) : "");
			awardExpenseTransactionVO.setPoDate(awardExpenseTransaction.getPoDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getPoDate().getTime()))) : "");
			awardExpenseTransactionVO.setVendorName(awardExpenseTransaction.getVendorName() != null ? awardExpenseTransaction.getVendorName() : "");
			awardExpenseTransactionVO.setFiGlDescription(awardExpenseTransaction.getFiGlDescription() != null ? awardExpenseTransaction.getFiGlDescription() : "");
			awardExpenseTransactionVO.setFiPostingDate(awardExpenseTransaction.getFiPostingDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getFiPostingDate().getTime()))) : "");
			awardExpenseTransactionVO.setInvoiceNumber(awardExpenseTransaction.getInvoiceNumber() != null ? awardExpenseTransaction.getInvoiceNumber() : "");
			awardExpenseTransactionVO.setBudgetCategory(getBudgetCategoryFromInternalOrderCode(awardExpenseTransaction.getInternalOrderCode()) != null ? getBudgetCategoryFromInternalOrderCode(awardExpenseTransaction.getInternalOrderCode()) : "");
			String budgetCategoryAcronym = getBudgetCategoryAcronymByCode(awardExpenseTransactionVO.getBudgetCategory(), budgetCategoryDetail);
			awardExpenseTransactionVO.setBudgetCategoryAcronym(budgetCategoryAcronym != null ? budgetCategoryAcronym : "");
			awardExpenseTransactionVO.setInternalOrderCode(awardExpenseTransaction.getInternalOrderCode() != null ? awardExpenseTransaction.getInternalOrderCode() : "");
			awardExpenseTransactionVO.setFiGlAccount(awardExpenseTransaction.getFiGlAccount() != null ? awardExpenseTransaction.getFiGlAccount() : "");
			awardExpenseTransactionVO.setCommitment(awardExpenseTransaction != null ? Constants.CLAIM_TEMPLATE_EXPENSE_COMMITMENT_VALUE : "");
			awardExpenseTransactionVO.setPredecessorDocNumber(awardExpenseTransaction.getPredecessorDocNumber() != null ? awardExpenseTransaction.getPredecessorDocNumber() : "");
			awardExpenseTransactionVO.setTransactionReferenceNumber(awardExpenseTransaction.getTransactionReferenceNumber() != null ? awardExpenseTransaction.getTransactionReferenceNumber() : "");
			if (summaryDetails != null && summaryDetail.getPeriodOfVisit() != null) {
				try {
					String travelFrom = summaryDetail.getPeriodOfVisit().substring(0, 10);
					String travelTo = summaryDetail.getPeriodOfVisit().substring(11, 21);
					Date travelFromDate = null;
					Date travelToDate = null;
					if (travelFrom != null && travelTo != null) {
						travelFromDate = dFormat.parse(travelFrom);
						String travelFromValue = String.valueOf(dateFormat.format(new Date(travelFromDate.getTime())));
						awardExpenseTransactionVO.setTravelFrom(travelFromValue);
						travelToDate = dFormat.parse(travelTo);
						String travelToValue = dateFormat.format(new Date(travelToDate.getTime()));
						awardExpenseTransactionVO.setTravelTo(travelToValue);
					}
				} catch (Exception e) {
					logger.error("Error while getting travel from and travel to date", e);
				}
			}
			try {
				commonDao.detachEntityFromSession(summaryDetail);
				if (summaryDetail.getManpowerPayrollId() != null) {
					summaryDetail.setQualifyingCost(summaryDetail.getEncryptedAmount() != null ? new BigDecimal(excelityService.decryptAESData(summaryDetail.getEncryptedAmount())) : null);
					summaryDetail.setTotalAmount(summaryDetail.getTotalAmount() == null ? summaryDetail.getQualifyingCost() : summaryDetail.getTotalAmount());
					summaryDetail.setAdjustedTotal(summaryDetail.getAdjustedTotal() == null ? summaryDetail.getQualifyingCost() : summaryDetail.getAdjustedTotal());
				}
			} catch (Exception e) {
				logger.error("error while decrypting amount", e);
			}
			awardExpenseTransactionVO.setAmount(summaryDetail.getAdjustedTotal());
			awardExpenseTransactionVOs.add(awardExpenseTransactionVO);
		});
		return awardExpenseTransactionVOs;
	}

	private List<AwardExpenseTransactionVO>  prepareExpenseForCommittedTransaction(String awardNumber, String accountNumber, Timestamp startDate,
			Timestamp endDate, String actualOrCommittedFlag, String templateTypeCode, String sponsorAwardNumber,
			Integer claimId) {
		List <AwardExpenseTransaction> awardExpenseTransactions = awardExpenseDao.getAwardExpenseCommittedTransactionForClaim(awardNumber, accountNumber, startDate, endDate);
		SimpleDateFormat dateFormat = new SimpleDateFormat(templateTypeCode.equals(Constants.CLAIM_IGMS_TEMPLATE_TYPE) ? "dd-MMM-yy" : "dd.MM.yyyy");
		List<AwardExpenseTransactionVO> awardExpenseTransactionVOs = new ArrayList<>();
		Map<String, String> budgetCategoryDetail= new HashMap<>();
		awardExpenseTransactions.forEach(awardExpenseTransaction -> {
			AwardExpenseTransactionVO awardExpenseTransactionVO = new AwardExpenseTransactionVO();
			awardExpenseTransactionVO.setAccountNumber(awardExpenseTransaction.getAccountNumber() != null ? awardExpenseTransaction.getAccountNumber() : "");
			awardExpenseTransactionVO.setAwardNumber(awardExpenseTransaction.getAwardNumber() != null ? awardExpenseTransaction.getAwardNumber() : "");
			awardExpenseTransactionVO.setSponsorAwardNumber(sponsorAwardNumber != null ? sponsorAwardNumber + "-01" : "");
			awardExpenseTransactionVO.setRemarks(awardExpenseTransaction.getRemarks() != null ? awardExpenseTransaction.getRemarks() : "");
			awardExpenseTransactionVO.setDocumentNumber(awardExpenseTransaction.getDocumentNumber() != null ? awardExpenseTransaction.getDocumentNumber() : "");
			awardExpenseTransactionVO.setPoNumber(awardExpenseTransaction.getPoNumber() != null ? awardExpenseTransaction.getPoNumber() : "");
			awardExpenseTransactionVO.setAmount(awardExpenseTransaction.getAmountInFmacurrency());
			awardExpenseTransactionVO.setFmPostingDate(awardExpenseTransaction.getFmPostingDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getFmPostingDate().getTime()))) : "");
			awardExpenseTransactionVO.setDocumentDate(awardExpenseTransaction.getDocumentDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getDocumentDate().getTime()))) : "");
			awardExpenseTransactionVO.setInvoiceDate(awardExpenseTransaction.getInvoiceDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getInvoiceDate().getTime()))) : "");
			awardExpenseTransactionVO.setPoDate(awardExpenseTransaction.getPoDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getPoDate().getTime()))) : "");
			awardExpenseTransactionVO.setVendorName(awardExpenseTransaction.getVendorName() != null ? awardExpenseTransaction.getVendorName() : "");
			awardExpenseTransactionVO.setFiGlDescription(awardExpenseTransaction.getFiGlDescription() != null ? awardExpenseTransaction.getFiGlDescription() : "");
			awardExpenseTransactionVO.setFiPostingDate(awardExpenseTransaction.getFiPostingDate() != null ? String.valueOf(dateFormat.format(new Date(awardExpenseTransaction.getFiPostingDate().getTime()))) : "");
			awardExpenseTransactionVO.setInvoiceNumber(awardExpenseTransaction.getInvoiceNumber() != null ? awardExpenseTransaction.getInvoiceNumber() : "");
			awardExpenseTransactionVO.setBudgetCategory(getBudgetCategoryFromInternalOrderCode(awardExpenseTransaction.getInternalOrderCode()) != null ? getBudgetCategoryFromInternalOrderCode(awardExpenseTransaction.getInternalOrderCode()) : "");
			String budgetCategoryAcronym = getBudgetCategoryAcronymByCode(awardExpenseTransactionVO.getBudgetCategory(), budgetCategoryDetail);
			awardExpenseTransactionVO.setBudgetCategoryAcronym(budgetCategoryAcronym != null ? budgetCategoryAcronym : "");
			awardExpenseTransactionVO.setInternalOrderCode(awardExpenseTransaction.getInternalOrderCode() != null ? awardExpenseTransaction.getInternalOrderCode() : "");
			awardExpenseTransactionVO.setFiGlAccount(awardExpenseTransaction.getFiGlAccount() != null ? awardExpenseTransaction.getFiGlAccount() : "");
			awardExpenseTransactionVO.setCommitment(!awardExpenseTransactions.isEmpty() ? Constants.CLAIM_TEMPLATE_EXPENSE_COMMITMENT_VALUE : "");
			awardExpenseTransactionVO.setPredecessorDocNumber(awardExpenseTransaction.getPredecessorDocNumber() != null ? awardExpenseTransaction.getPredecessorDocNumber() : "");
			awardExpenseTransactionVO.setTransactionReferenceNumber(awardExpenseTransaction.getTransactionReferenceNumber() != null ? awardExpenseTransaction.getTransactionReferenceNumber() : "");
			awardExpenseTransactionVOs.add(awardExpenseTransactionVO);
		});
		return awardExpenseTransactionVOs;		
	}

	private String getBudgetCategoryAcronymByCode(String budgetCategoryCode, Map<String, String> budgetCategoryDetail) {
		String budgetCategoryAcronym = null;
		if (!budgetCategoryDetail.containsKey(budgetCategoryCode)) {
			BudgetCategory budgetCategory = budgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
			budgetCategoryDetail.put(budgetCategoryCode, budgetCategory.getBudgetCategoryAcronym());
			budgetCategoryAcronym = budgetCategory.getBudgetCategoryAcronym();
		} else {
			budgetCategoryAcronym =  budgetCategoryDetail.get(budgetCategoryCode);
		}
		return budgetCategoryAcronym;
	}

	private String getBudgetCategoryFromInternalOrderCode(String internalOrderCode) {
		return internalOrderCode.substring(15,18);
	}

	private void prepareZipFile(ClaimsVO vo, HttpServletResponse response, String fileName, List<Object> templateTypeCodes) {
		String claimNumber = vo.getClaim().getClaimNumber();
		Claim claim = vo.getClaim();
		response.setContentType("application/zip");
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ZipOutputStream zos = new ZipOutputStream(baos);
		ServletOutputStream op = null;
		ByteArrayOutputStream outputStream = null;
		try {
			SimpleDateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
			vo.setLastClaimEndDate(claimsDao.getLastClaimEndDate(vo.getClaim().getAwardNumber(), vo.getClaimId()));
			if (vo.getLastClaimEndDate() != null) {
				vo.setPreviousClaimEndDate(String.valueOf(dateFormat.format(new Date(vo.getLastClaimEndDate().getTime()))));
			}
			if (claim.getEndDate() != null) {
				vo.setClaimEndDate(String.valueOf(dateFormat.format(new Date(claim.getEndDate().getTime()))));
			}
			if (claim.getStartDate() != null && claim.getEndDate() != null) {
				claim.setDurationInMonths(getDurationInMonths(new Timestamp(claim.getStartDate().getTime()), new Timestamp(claim.getEndDate().getTime())));
			}
			if(claim.getAward() != null && claim.getAward().getBeginDate() != null && claim.getAward().getFinalExpirationDate() != null) {
				claim.getAward().setDurationInMonths(getDurationInMonths(new Timestamp(claim.getAward().getBeginDate().getTime()),
						new Timestamp(claim.getAward().getFinalExpirationDate().getTime())));
			}
			vo.setoHPercentage(setOhPercentage(claimsDao.getOverheadPercentage(vo.getClaimId())));
			if(claim.getAward().getGrantHeaderId() != null) {
			vo.setClaimFundingScheme(claimsDao.getSponsorFundingSchemeByGrantId(claim.getAward().getGrantHeaderId()));
			}
			prepareMainSheet(vo);
			prepareDetailBreakDownSheet(vo);
			loadClaimAdvance(vo);
			prepareManpowerSheet(vo);
			getAllClaimsForAward(vo); // year based claim details for a award
			vo.setClaim(claim);
			for (Object templateTypeCodeObj : templateTypeCodes) {
				outputStream = new ByteArrayOutputStream();
				String templateTypeCode = (String) templateTypeCodeObj;
				byte[] bFile = printService.getTemplateData(templateTypeCode);
				InputStream inputStream = new ByteArrayInputStream(bFile);
				LetterTemplateType letterTemplate = printService.getFileNameByTemplateCode(templateTypeCode);
				String templateName = FilenameUtils.getBaseName(letterTemplate.getFileName());
				String printFileType = letterTemplate.getPrintFileType();
				Context context = new Context();
				SimpleDateFormat dateFormat_MMM_yyyy = new SimpleDateFormat("MMM yyyy");
				context.putVar("dateFormat_MMM_yyyy", dateFormat_MMM_yyyy);
				SimpleDateFormat dateFormat_MMM_yy = new SimpleDateFormat("MMM yy");
				context.putVar("dateFormat_MMM_yy", dateFormat_MMM_yy);
				SimpleDateFormat dateFormat_dd_MM_yy = new SimpleDateFormat("dd MM yy");
				context.putVar("dateFormat_dd_MM_yy", dateFormat_dd_MM_yy);
				SimpleDateFormat dateFormat_dd_MM_yyyy = new SimpleDateFormat("dd MM yyyy");
				context.putVar("dateFormat_dd_MM_yyyy", dateFormat_dd_MM_yyyy);
				SimpleDateFormat dateFormat_YYYY_MM = new SimpleDateFormat("YYYY/MM");
				context.putVar("dateFormat_YYYY_MM", dateFormat_YYYY_MM);
				SimpleDateFormat dateFormat_ddMMyy = new SimpleDateFormat("dd/MM/yy");
				context.putVar("dateFormat_ddMMyy", dateFormat_ddMMyy);
				SimpleDateFormat dateFormat_dd_MMM_yyyy = new SimpleDateFormat("dd MMM yyyy");
				context.putVar("dateFormat_dd_MMM_yyyy", dateFormat_dd_MMM_yyyy);
				SimpleDateFormat dateFormat_ddMMMyy = new SimpleDateFormat("ddMMMyy");
				context.putVar("dateFormat_ddMMMyy", dateFormat_ddMMMyy);
				context.putVar("claimFilter", new JxlsComparator());
				context.putVar("aggregator", new JxlsAggregator());
				context.putVar("date", new JxlsDateFormatter());
				if (printFileType.equals("csv")) {
					if (Constants.CLAIM_ASTAR_COMMITTED_TEMPLATE_EQPT.equals(templateTypeCode) || Constants.CLAIM_ASTAR_COMMITTED_TEMPLATE_OOE.equals(templateTypeCode)) {
						vo.setActualOrCommitedFlag("C");
					} else {
						vo.setActualOrCommitedFlag("A");
					}
					List<AwardExpenseTransactionVO> awardExpenseTransactionVOs = prepareAwardExpenseTransactions(
							claim.getAwardNumber(), claim.getAccountNumber(), new Timestamp(claim.getStartDate().getTime()), new Timestamp(claim.getEndDate().getTime()),
							vo.getActualOrCommitedFlag(), templateTypeCode, claim.getAward().getSponsorAwardNumber(), claim.getClaimId());
					if (awardExpenseTransactionVOs.isEmpty()) {
						awardExpenseTransactionVOs.add(new AwardExpenseTransactionVO());
					}
					vo.setAwardExpenseTransactions(awardExpenseTransactionVOs);
					context.putVar(CLAIMVO, vo);
					logger.info("Applying at cell Details for prepareClaimTemplate");
					JxlsHelper.getInstance().processTemplate(inputStream, outputStream, context);
					logger.info("written to csv file");
					byte[] claimTemplateData = outputStream.toByteArray();
					claimTemplateData = convertXlsxToCsvFile(claimTemplateData);
					setTemplateDetail(claimTemplateData, claimNumber + "_" + templateName + ".csv", zos);
					outputStream.close();
				} else if (printFileType.equals("xlsx")) {
					context.putVar(CLAIMVO, vo);
					logger.info("Applying at cell Details for prepareClaimTemplate");
					JxlsHelper.getInstance().processTemplate(inputStream, outputStream, context);
					logger.info("written to xlsx file");
					byte[] claimTemplateData = outputStream.toByteArray();
					setTemplateDetail(claimTemplateData, claimNumber + "_" + templateName + ".xlsx", zos);
					outputStream.close();
				}
			}
			byte[] generalTemplateData = prepareClaimExcelSheet(vo, Constants.CLAIM_GENERIC_EXCEL_LETTER_TEMPLATE_TYPE, claim);
			setTemplateDetail(generalTemplateData, claimNumber + "_Claim_General_Report.xlsx", zos);
			baos.flush();
			zos.close();
			baos.close();
			op = response.getOutputStream();
			op.write(baos.toByteArray());
			op.flush();
			op.close();
		} catch (Exception e) {
			logger.error("Exception in prepareZipFile : {}", e.getMessage());
			throw new ApplicationException("prepareZipFile", e, Constants.JAVA_ERROR);
		} finally {
			try {
				zos.close();
				baos.close();
				op.close();
			} catch (IOException e) {
				logger.error("Exception in closing ByteArray : {}", e.getMessage());
			}
		}
	}

	private void getAllClaimsForAward(ClaimsVO vo) {
		Integer count = 0;
		vo.setAwardNumber(vo.getClaim().getAwardNumber());
		loadAllSubmittedClaimsForTemplate(vo);
		if (vo.getClaims() != null) {
			List<Claim> claims = vo.getClaims().stream().sorted(Comparator.comparing(Claim::getClaimId)).collect(Collectors.toList());
			for (Claim claim : claims) {
				ClaimsVO claimsVO = new ClaimsVO();
				claimsVO.setClaim(claim);
				claimsVO.setClaimId(claim.getClaimId());
				prepareMainSheet(claimsVO);
				claim.setSummaryCalculations(setClaimSummaryCalculations(claimsVO.getClaimSummary(), claimsVO.getOverHeadPercentage(), claim.getClaimId()));
				claim.setClaimSummaryList(claimsVO.getClaimSummary());
				claim.setSummaryOOEMACSumValues(claimsVO.getSummaryOOE_MACSumValues());
				setBudgetCatergoryDummyValues(claim);
				count++;
				claim.setClaimIndex(count);
				claim.setQuarter(findClaimQuarter(claim.getStartDate()));
				claim.setClaimHalfYearlyPeriod(findHalfYearlyPeriod(claim.getStartDate()));
				claim.setFinancialYear(claim.getClaimNumber().substring(1, 5));
				claim.setClaimSummaryDetails(loadClaimSummaryDetailsByClaimId(claim.getClaimId(), vo));
			}
		}
	}

	private String loadAllSubmittedClaimsForTemplate(ClaimsVO claimsVO) {
		List<Object[]> claimsObject = claimsDao.loadClaimsForAward(claimsVO.getAwardNumber());
		List<Claim> claims = new ArrayList<>();
		AtomicInteger dateEqualCount = new AtomicInteger(0);
		claimsObject.forEach(claimObject -> {
			if (dateEqualCount.compareAndSet(0, 0)) {
				Claim claim = new Claim();
				claim.setClaimId(Integer.parseInt(claimObject[0].toString()));
				claim.setClaimNumber(claimObject[1].toString());
				claim.setAwardId(Integer.parseInt(claimObject[2].toString()));
				claim.setAwardNumber(claimObject[3].toString());
				try {
					Date startDate = new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[4].toString());
					Date endDate = new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[5].toString());
					Date submissionDate = claimObject[6] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[6].toString()) : null;
					Date documentDate = claimObject[11] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[11].toString()) : null;
					Date paymentDate = claimObject[12] != null ? new SimpleDateFormat("dd/MM/yyyy").parse(claimObject[12].toString()) : null;
					claim.setStartDate(startDate);
					claim.setEndDate(endDate);
					claim.setClaimSubmissionDate(submissionDate);
					claim.setDocumentDate(documentDate);
					claim.setPaymentDate(paymentDate);
				} catch (Exception e) {
					logger.error("error occured while parsing date in award claims", e);
				}
				String claimStartDate = new SimpleDateFormat(Constants.EXCEL_DATE_FORMAT).format(new Date(claim.getStartDate().getTime()));
				String claimEndDate = new SimpleDateFormat(Constants.EXCEL_DATE_FORMAT).format(new Date(claim.getEndDate().getTime()));
				String startAndEndYear = claimStartDate + " to " + claimEndDate;
				if (claimsVO.getClaimStartAndEndYear().equals(startAndEndYear)) {
					dateEqualCount.getAndIncrement();
				}
				claim.setClaimStartAndEndYear(startAndEndYear);
				ClaimStatus claimStatus = new ClaimStatus();
				claimStatus.setClaimStatusCode(claimObject[7].toString());
				claimStatus.setDescription(claimObject[8].toString());
				claim.setClaimStatus(claimStatus);
				claim.setTotalAmount(new BigDecimal(claimObject[9].toString()));
				claim.setOutputDocNumber(claimObject[10] != null ? claimObject[10].toString() : null);
				claims.add(claim);				
			}
		});
		claimsVO.setClaims(claims);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private void setBudgetCatergoryDummyValues(Claim claim) {
		BigDecimal directCostSubTotal = BigDecimal.ZERO;
		BigDecimal indirectCostClaimed = BigDecimal.ZERO;
		BigDecimal researchScholarship = BigDecimal.ZERO;
		BigDecimal directAndIndirectClaimed = BigDecimal.ZERO;
		if (claim.getClaimSummaryList() != null) {
			List<String> budgetCategory = new ArrayList<String>();
			budgetCategory.add("EOM");
			budgetCategory.add("EQT");
			budgetCategory.add("OOE");
			budgetCategory.add("OST");
			budgetCategory.add("RSS");
			for (ClaimSummary claimSummary : claim.getClaimSummaryList()) {
				budgetCategory.removeIf(e -> e.contains(claimSummary.getBudgetCategoryCode()));
				if(!claimSummary.getBudgetCategoryCode().equals("EXI")) {
				directCostSubTotal = directCostSubTotal.add(claimSummary.getAmountRequested());
				}
				if (claimSummary.getBudgetCategoryCode().equals("RSS")) {
					researchScholarship = claimSummary.getAmountRequested();
				}
			}
			indirectCostClaimed = (new BigDecimal("0.2").multiply(directCostSubTotal.subtract(researchScholarship))).setScale(2, RoundingMode.HALF_UP);
			directAndIndirectClaimed = directCostSubTotal.add(indirectCostClaimed);
			claim.setTotalDirectAndIndirectClaimed(directAndIndirectClaimed);
			claim.setIndirectCostOverHead(indirectCostClaimed);
			claim.setSubTotalDirectCost(directCostSubTotal);
			List<ClaimSummary> claimsList = new ArrayList<>();
			for (String categoryCode : budgetCategory) {
				ClaimSummary claimSummary = new ClaimSummary();
				claimSummary.setBudgetCategoryCode(categoryCode);
				claimSummary.setAmountRequested(BigDecimal.ZERO);
				claimsList.add(claimSummary);
			}
			List<ClaimSummary> claimSummaryFinalList = claim.getClaimSummaryList();
			claimSummaryFinalList.addAll(claimsList);
		}
	}

	private byte[] convertXlsxToCsvFile(byte[] data) {
		StringBuilder byteArray = new StringBuilder();
		InputStream inputStream = new ByteArrayInputStream(data);
		try {
			Workbook workbook = WorkbookFactory.create(inputStream);
			Sheet sheet = workbook.getSheetAt(0);
			Row row = null;
			for (int i = 0; i < sheet.getLastRowNum() + 1; i++) {
				row = sheet.getRow(i);
				StringBuilder rowDetail = new StringBuilder();
				if (sheet.getRow(i) != null && sheet.getRow(i).getPhysicalNumberOfCells() > 0) {
					for (int j = 0; j < sheet.getRow(i).getPhysicalNumberOfCells(); j++) {
						String newData = null;
						if(row.getCell(j) != null)
							newData = escapeSpecialCharactersFromData(row.getCell(j).toString());
						if (i == 0 && newData != null) {
							rowDetail = rowDetail.append(newData).append(",");
						} else {
							if (row.getCell(j) == null) {
								rowDetail = rowDetail.append('"').append('"').append(',');
							} else {
								rowDetail = rowDetail.append(newData).append(",");
							}
						}
					}
					byteArray = byteArray.append(rowDetail.substring(0, rowDetail.length() - 1)).append("\n");
				}
			}
			workbook.close();
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in convertXlsxToCsvFile : {}", e.getMessage());
		} finally {
			if(inputStream != null) {
				try {
					inputStream.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return byteArray.toString().getBytes(StandardCharsets.UTF_8);
	}

	public String escapeSpecialCharactersFromData(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    escapedData =  escapedData.replaceAll("[\\r\\n]+", " ");
	    if (escapedData.contains(",") || escapedData.contains("\"") || escapedData.contains("'")) {
	    	escapedData = escapedData.replace("\"", "\"\"");
	        escapedData = "\"" + escapedData + "\"";
	    }
	    return escapedData;
	}
	
	public void setTemplateDetail(byte[] generalTemplateData, String fileName, ZipOutputStream zos) {
		if (generalTemplateData != null && generalTemplateData.length > 0) {
			try {
				zos.putNextEntry(new ZipEntry(fileName));
				zos.write(generalTemplateData);
				zos.closeEntry();
				zos.flush();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public String updateClaimDuration() {
		List<Claim> claims = claimsDao.getClaimsWithoutDuration();
		claims.forEach(claim -> {
			claim.setDuration(getDurationValue(new Timestamp(claim.getStartDate().getTime()), new Timestamp(claim.getEndDate().getTime())));
		});
		Map<String, Object> result = new HashMap<>();
		result.put("status", "updated");
		return commonDao.convertObjectToJSON(result);
	}
	
	private String getDurationInMonths(Timestamp startTime, Timestamp endTime) {
		String durationMonths = "";
		try {
			Date startDate = commonDao.adjustTimezone(new Date(startTime.getTime()));
			Date endDate = commonDao.adjustTimezone(new Date(endTime.getTime()));
			LocalDate localStartDate = new Timestamp(startDate.getTime()).toLocalDateTime().toLocalDate();
			LocalDate localEndDate = new Timestamp(endDate.getTime()).toLocalDateTime().toLocalDate().plusDays(1);
			Period difference = Period.between(localStartDate, localEndDate);
			durationMonths = difference.toTotalMonths()+ " month(s)";  
		} catch (Exception e) {
			logger.error("Exception in getDurationInMonths : {}", e.getMessage());
		}
		return durationMonths;
	}

	private String getDurationValue(Timestamp startTime, Timestamp endTime) {
		String duration = "";
		try {
			Date startDate = commonDao.adjustTimezone(new Date(startTime.getTime()));
			Date endDate = commonDao.adjustTimezone(new Date(endTime.getTime()));
			LocalDate localStartDate = new Timestamp(startDate.getTime()).toLocalDateTime().toLocalDate();
			LocalDate localEndDate = new Timestamp(endDate.getTime()).toLocalDateTime().toLocalDate().plusDays(1);
			Period difference = Period.between(localStartDate, localEndDate);
			duration = difference.getYears() + " year(s), " + difference.getMonths() + " month(s) & "
					+ difference.getDays() + " day(s)";
		} catch (Exception e) {
			logger.error("Exception in getDurationValue : {}", e.getMessage());
		}
		return duration;
	}
	
	@Override
	public String saveOrUpdateClaimInvoiceDetail(ClaimsVO claimsVO) {
		claimsDao.saveOrUpdateClaimInvoiceDetail(claimsVO.getClaimInvoiceDetail());
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public String loadClaimInvoice(Integer claimId) {
		ClaimsVO claimsVO = new ClaimsVO();
		ClaimInvoice claimInvoice = claimsDao.loadClaimInvoice(claimId);
		Map<String, List<ClaimOutputGstTaxCode>> mapClaimOutputGstTaxCode =  claimsDao.getClaimOutputGstTaxCodeInTaxCode(claimInvoice.getClaimInvoiceDetails().stream().filter(details -> details.getTaxCode() != null)
				.map(ClaimInvoiceDetails :: getTaxCode).collect(Collectors.toSet())).stream().collect(Collectors.groupingBy(ClaimOutputGstTaxCode :: getTaxCode));
		claimInvoice.getClaimInvoiceDetails().forEach(invoiceDetails -> {
			if(mapClaimOutputGstTaxCode.containsKey(invoiceDetails.getTaxCode()))
				invoiceDetails.setClaimOutputGstTaxCode(mapClaimOutputGstTaxCode.get(invoiceDetails.getTaxCode()).get(0));
		});
		Claim claim = claimsDao.getClaim(claimId);
		claimInvoice.setCampus(claimsDao.getCampusForUnit(claim.getAward().getLeadUnitNumber()));
		if(claimInvoice.getDocumentTypeCode() != null)
			claimInvoice.setClaimInvoiceMetadata(claimsDao.getClaimInvoiceMetadataByParams(claimInvoice.getCampus(), claimInvoice.getDocumentTypeCode(), false));
		getClaimInvoiceSetupData(claim, claimId, claimInvoice, claimsVO);
		claimInvoice.setBaCode(claimsDao.getBACodeForUnit(claim.getAward().getLeadUnitNumber()));
		claimsVO.setClaimInvoice(claimInvoice);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private void getClaimInvoiceSetupData(Claim claim, Integer claimId, ClaimInvoice claimInvoice, ClaimsVO claimsVO) {
		claimInvoice.setClaimAmount(claimsDao.getClaimAmount(claim.getAwardNumber(), claimId));
		claimsVO.setCustomDataView(claimsDao.getAwardCustomData(claim.getAwardId()));
		if(claimsVO.getCustomDataView() != null && claimsVO.getCustomDataView().getOutputGstCategory() != null)
			claimsVO.setClaimOutputGstTaxCode(claimsDao.getClaimOutputGstTaxCode(claimsVO.getCustomDataView().getOutputGstCategory()));	
	}

	@Override
	public String loadClaimInvoiceLookups() {
		Map<String, Object> result = new HashMap<>();
		List<ClaimInvoiceMetadata> claimInvoiceMetadatas = claimsDao.getClaimInvoiceMetadata();
		Map<String, List<ClaimInvoiceMetadata>> baCodeMetadata = claimInvoiceMetadatas.stream()
				.collect(Collectors.groupingBy(ClaimInvoiceMetadata::getBaCode));
		result.put("baCodeMetadata", baCodeMetadata);
		result.put("glAccountCode", claimsDao.loadGlAccountCodes());
		result.put("taxCodes", claimsDao.loadClaimTaxCodes());
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String saveOrUpdateClaimInvoice(ClaimsVO claimsVO) {
		claimsDao.saveOrUpdateClaimInvoice(claimsVO.getClaimInvoice());
		return commonDao.convertObjectToJSON(claimsVO);
	}

	@Override
	public String deleteClaimInvoiceDetail(Integer invoiceDetailId) {
		claimsDao.deleteClaimInvoiceDetail(invoiceDetailId);
		Map<String, Boolean> result = new HashMap<>();
		result.put("status", true);
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String loadClaimInvoiceSummary(Integer claimId) {
		ClaimsVO claimsVO = new ClaimsVO();
		List<ClaimInvoiceLog> claimInvoice = claimsDao.loadAllClaimInvoice(claimId);
		Claim claim = claimsDao.getClaim(claimId);
		String baCode = claimsDao.getCampusForUnit(claim.getAward().getLeadUnitNumber());
		Map<String, List<ClaimOutputGstTaxCode>> mapClaimOutputGstTaxCode =  claimsDao.loadClaimTaxCodes()
				.stream().collect(Collectors.groupingBy(ClaimOutputGstTaxCode :: getTaxCode));
		claimInvoice.forEach(claimInv -> {
			claimInv.getClaimInvoiceDetails().forEach(invoiceDetails -> {
				if(mapClaimOutputGstTaxCode.containsKey(invoiceDetails.getTaxCode()))
					invoiceDetails.setClaimOutputGstTaxCode(mapClaimOutputGstTaxCode.get(invoiceDetails.getTaxCode()).get(0));
				BigDecimal claimAmount = invoiceDetails.getClaimAmount() != null ? invoiceDetails.getClaimAmount() : new BigDecimal(0.00);
				BigDecimal subContractAmount = invoiceDetails.getSubContractAmount() != null ? invoiceDetails.getSubContractAmount() : new BigDecimal(0.00);
				BigDecimal totalAmount =  claimAmount.add(subContractAmount);
				claimInv.setClaimAmount(claimInv.getClaimAmount() == null ? totalAmount : claimInv.getClaimAmount().add(totalAmount));
			});
			claimInv.setClaimInvoiceMetadata(claimsDao.getClaimInvoiceMetadataByParams(baCode, claimInv.getDocumentTypeCode(), (claimInv.getTypeCode() != null && claimInv.getTypeCode().equals(Constants.CLAIM_FEED_TYPE_CREDIT))));
		});
		List<List<ClaimInvoiceLog>> groupedClaimInvoices = claimInvoice.stream().collect(Collectors.groupingBy(ClaimInvoiceLog :: getSequenceNumber,LinkedHashMap::new,Collectors.toList())).values().stream().collect(Collectors.toList());		
		claimsVO.setClaimInvoiceVersions(groupedClaimInvoices);
		return commonDao.convertObjectToJSON(claimsVO);
	}
	
	private void createNewClaimInvoiceVersion(Integer claimId) {
		ClaimInvoice claimInvoice = claimsDao.loadClaimInvoice(claimId);
		ClaimInvoice newClaimInvoice = new ClaimInvoice();
		BeanUtils.copyProperties(claimInvoice, newClaimInvoice);
		newClaimInvoice.setInvoiceId(null);
		newClaimInvoice.setSequenceNumber(claimInvoice.getSequenceNumber()+1);
		claimsDao.saveOrUpdateClaimInvoice(newClaimInvoice);
		List<ClaimInvoiceDetails> claimInvoiceDetails = new ArrayList<>();
		claimInvoice.getClaimInvoiceDetails().forEach(invoiceDetails -> {
			ClaimInvoiceDetails details = new ClaimInvoiceDetails();
			BeanUtils.copyProperties(invoiceDetails, details);
			details.setInvoiceDetailId(null);
			details.setInvoiceId(newClaimInvoice.getInvoiceId());
			details.setClaimInvoice(newClaimInvoice);
			claimInvoiceDetails.add(details);
		});
		newClaimInvoice.setClaimInvoiceDetails(claimInvoiceDetails);	
		claimsDao.saveOrUpdateClaimInvoice(newClaimInvoice);
	}

	@Override
	public String loadClaimInvoiceSapResponse(Integer claimId, Integer sequenceNumber) {
		Map<String, List<SapClaimFeedResponseMessage>> response  = new HashMap<String, List<SapClaimFeedResponseMessage>>();
		response.put("sapMessages", claimsDao.loadClaimInvoiceSapResponse(claimId, sequenceNumber));
		return commonDao.convertObjectToJSON(response);
	}

	@Override
	public String deleteClaimDetail(ClaimsVO claimsVO) {
		ClaimsVO vo = new ClaimsVO();
		List<String> rightNames = new ArrayList<>();
		rightNames.add(CLAIM_PREPARER);
		rightNames.add(DELETE_CLAIM);
		Boolean isPersonHasRightPermission = commonDao.checkPersonHasRightInModule(Constants.MODULE_CODE_AWARD, claimsVO.getAwardId(), rightNames, AuthenticatedUser.getLoginPersonId());
		if (Boolean.FALSE.equals(isPersonHasRightPermission)) {
			isPersonHasRightPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(),CLAIM_PREPARER, claimsVO.getAwardLeadUnitNumber());
			if ((Boolean.FALSE.equals(isPersonHasRightPermission))) {
				isPersonHasRightPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(),DELETE_CLAIM, claimsVO.getAwardLeadUnitNumber());
			}
		}
		if ((isPersonHasRightPermission != null && Boolean.TRUE.equals(isPersonHasRightPermission))) {
			claimsDao.deleteClaimDetails(claimsVO.getClaimId());
			vo.setStatus(Boolean.TRUE);
		} else {
			vo.setStatus(Boolean.FALSE);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override 
	public String resyncClaimDetail(ClaimsVO claimsVO) {
		Claim claim = claimsDao.getClaim(claimsVO.getClaimId());
		List<ClaimSummary> claimSummary = claimsDao.loadClaimSummary(claim.getClaimId());
		BigDecimal totalCommitmentsUptoClaim = BigDecimal.ZERO;
		BigDecimal totalAmountForecasted = BigDecimal.ZERO;
		for (ClaimSummary summary : claimSummary) {
			if (summary.getBudgetCategoryCode().equals("EOM")) {
				summary.setAmountRequested(claimsDao.getClaimSummaryAmountReq(claim.getClaimId(), "EOM"));
			} else if(summary.getBudgetCategoryCode().equals("RSS")) {
				summary.setAmountRequested(claimsDao.getClaimSummaryAmountReq(claim.getClaimId(), "RSS"));
			}			
			BigDecimal cumAmountUntilLastClaim = BigDecimal.ZERO;
			BigDecimal cumExpenseUptoPrevClaimPeriod = BigDecimal.ZERO;
			ClaimSummary previousClaimSummary = claimsDao.getPrevClaimSummaryOfBudgetCategory(claim.getClaimId(), claim.getAwardNumber(), summary.getBudgetCategoryCode());
			if (previousClaimSummary != null) {
				BigDecimal amountRequiredInPrevClaim = previousClaimSummary.getAmountReqForCurrentClaim() == null ? BigDecimal.ZERO : previousClaimSummary.getAmountReqForCurrentClaim();
				ClaimFundingScheme fundingScheme = claimsDao.getClaimFundingScheme(claim.getAwardId());
				if(fundingScheme != null && fundingScheme.getOverrideNegativeAmount() != null && fundingScheme.getOverrideNegativeAmount().equals(Boolean.TRUE))
					amountRequiredInPrevClaim = new BigDecimal(Math.max(Double.valueOf(amountRequiredInPrevClaim.toString()), 0));
				cumAmountUntilLastClaim = (previousClaimSummary.getCumClaimAmountUptoClaim() == null ? BigDecimal.ZERO: previousClaimSummary.getCumClaimAmountUptoClaim()).add(amountRequiredInPrevClaim);
				cumExpenseUptoPrevClaimPeriod = previousClaimSummary.getCumExpenseUptoPrevClaim() == null ? BigDecimal.ZERO: previousClaimSummary.getCumExpenseUptoPrevClaim()
						.add(previousClaimSummary.getAmountRequested() == null ? BigDecimal.ZERO : previousClaimSummary.getAmountRequested());												   
			}
			BigDecimal commitmentsAmtUptoPeriod = BigDecimal.ZERO;
			BigDecimal totalExpIncuredUptoClaim = cumExpenseUptoPrevClaimPeriod.add(summary.getAmountRequested() == null ? BigDecimal.ZERO : summary.getAmountRequested());
			BigDecimal fundBalanceAtEndOfClaim = cumAmountUntilLastClaim.subtract(totalExpIncuredUptoClaim);
			BigDecimal amountReqForCurrentClaim = commitmentsAmtUptoPeriod.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted()).subtract(fundBalanceAtEndOfClaim);
			BigDecimal prevClaimsTotalAmount =  claimsDao.getPreviousClaimsTotalAmountById(claimsVO.getClaimId(), claim.getAwardNumber(), summary.getBudgetCategoryCode());
			summary.setPrevClaimsTotalAmount(prevClaimsTotalAmount);
			summary.setCumClaimAmountUptoClaim(cumAmountUntilLastClaim);
			summary.setCumExpenseUptoPrevClaim(cumExpenseUptoPrevClaimPeriod);
			summary.setCommitmentsUptoPrevClaim(commitmentsAmtUptoPeriod);
			summary.setAmountReqForCurrentClaim(amountReqForCurrentClaim);
			claimsDao.saveOrUpdateClaimSummary(summary);
			totalCommitmentsUptoClaim = totalCommitmentsUptoClaim.add(summary.getCommitmentsUptoPrevClaim() == null ? BigDecimal.ZERO : summary.getCommitmentsUptoPrevClaim());
			totalAmountForecasted = totalAmountForecasted.add(summary.getAmountForcasted() == null ? BigDecimal.ZERO : summary.getAmountForcasted());
		}
		BigDecimal overheadPercentage = claim.getOverHeadPercentage() == null ? BigDecimal.ZERO : claim.getOverHeadPercentage();
		claim.setIdcAmountForcasted(totalAmountForecasted.multiply(overheadPercentage.divide(new BigDecimal(100))));
		claim.setIdcCommitmentUptoPrevClaim(totalCommitmentsUptoClaim.multiply(overheadPercentage.divide(new BigDecimal(100))));	
		claim.setIdcCumClaimAmtUptoClaim(getIndirectCostCumClaimAmountUptoPrevClaim(claim.getClaimId(), claim.getAwardNumber()));
		claimsDao.saveOrUpdateClaim(claim);
		claimsVO.setClaim(claim);
		return commonDao.convertObjectToJSON(claimsVO);
	}

	private List<LinkedHashMap<Object, Object>> loadClaimSummaryDetailsByClaimId(Integer claimId, ClaimsVO claimsVO) {
		Boolean isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.CLAIM_MAINTAIN_DETAILED_BREAKDOWN, claimsVO.getLeadUnitNumber());
		if(isPersonHasPermission.equals(Boolean.FALSE))
			return new ArrayList<>();
		List<ClaimSummaryDetails>  initialSummaryDetails =  claimsDao.loadClaimDetailBreakDown(claimId);
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> summary.getBudgetCategoryCode().equals("EOM") || summary.getBudgetCategoryCode().equals("RSS")).forEach(summary -> {
			try {
				commonDao.detachEntityFromSession(summary);
				if (summary.getManpowerPayrollId() != null) {
					summary.setQualifyingCost(summary.getEncryptedAmount() != null ? new BigDecimal(excelityService.decryptAESData(summary.getEncryptedAmount())) : null);
					summary.setTotalAmount(summary.getTotalAmount() == null ? summary.getQualifyingCost() : summary.getTotalAmount());
					summary.setAdjustedTotal(summary.getAdjustedTotal() == null ? summary.getQualifyingCost() : summary.getAdjustedTotal());
				} else {
					setInvolvmentDate(summary);
				}
				summary.setDescriptionOfExpenditure(summary.getAwardExpenseTransaction().getRemarks());
				summary.setExpenseCode(summary.getAwardExpenseTransaction().getInternalOrderCode().substring(15));
				claimsDao.getBudgetDetails(summary);
			} catch (Exception e) {
				logger.error("error while decrypting amount", e);
			}
		});
		initialSummaryDetails.stream().filter(summary -> summary.getAwardExpenseTransaction() != null).filter(summary -> !summary.getBudgetCategoryCode().equals("EOM")
				&& !summary.getBudgetCategoryCode().equals("RSS")).forEach(summary -> {
			summary.setDescriptionOfExpenditure(summary.getAwardExpenseTransaction().getRemarks());
			commonDao.detachEntityFromSession(summary);
			claimsDao.getBudgetDetails(summary);
		});
		getManpowerdetails(initialSummaryDetails);
		List<LinkedHashMap<Object, Object>> finalResult = new ArrayList<>();
		List<BigDecimal> quarterTotalAmount = new ArrayList<>();
		List<BigDecimal> quarterAdjustedTotal = new ArrayList<>();
		LinkedHashMap<String, List<ClaimSummaryDetails>> summaryGroups = initialSummaryDetails.stream()
				.sorted(Comparator.comparing(ClaimSummaryDetails::getBudgetCategoryCode))
				.collect(Collectors.groupingBy(summaryDetails -> summaryDetails.getClaimSummary().getBudgetCategoryCode(),LinkedHashMap::new,Collectors.toList()));
		summaryGroups.values().forEach(summaryBudgetDetails -> {
			LinkedHashMap<Object, List<ClaimSummaryDetails>> summaryGroupsInternal = summaryBudgetDetails.stream()
					.sorted(Comparator.comparing(summaryDetails -> summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes()))
					.collect(Collectors.groupingBy(summaryDetails -> summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes(),LinkedHashMap::new,Collectors.toList()));
			summaryGroupsInternal.values().forEach(summaryDetailsList -> {
				LinkedHashMap<Object, Object> result = new LinkedHashMap<>();
				summaryDetailsList.stream().sorted(Comparator.comparing(ClaimSummaryDetails::getClaimDetailsId)).collect(Collectors.toList()).forEach(summaryDetails -> {
					result.put("budgetCategory", summaryDetails.getClaimSummary().getBudgetCategory());
					result.put("internalOrderCode", summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getInternalOrderCode() : summaryDetails.getInternalOrderCodes());
					result.put("approvedBudget", summaryDetails.getApprovedBudget());
					if (summaryDetails.getApprovedHeadCount() != null)
						result.put("approvedHeadCount", summaryDetails.getApprovedHeadCount());
					if (summaryDetails.getActualHeadCount() != null)
						result.put("actualHeadCount", summaryDetails.getActualHeadCount());
					result.put("awardNumber", summaryDetails.getAwardExpenseTransaction() != null ? summaryDetails.getAwardExpenseTransaction().getAwardNumber() : summaryDetails.getClaimSummary().getClaim().getAwardNumber());
					if (Boolean.TRUE.equals(claimsVO.getIsExcelExport())) {
						if (summaryDetails.getApprovedHeadCount() != null)
							result.put("approvedHeadCountData", summaryDetails.getClaimSummary().getBudgetCategory().getCode().equals("EOM") && summaryDetails.getApprovedHeadCount() != null ? "Approved HeadCount : "+ summaryDetails.getApprovedHeadCount().toString() : "");
						if (summaryDetails.getActualHeadCount() != null)
							result.put("actualHeadCountData", summaryDetails.getClaimSummary().getBudgetCategory().getCode().equals("EOM") && summaryDetails.getActualHeadCount() != null ? "Actual HeadCount : "+ summaryDetails.getActualHeadCount().toString() : "");
						result.put("approvedBudgetData", summaryDetails.getApprovedBudget() != null ? new StringBuilder("Approved Budget : ").append(new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL).format(summaryDetails.getApprovedBudget())).toString() : "");
					}
				});
				List<Map<Object, Object>> finalTransactionResult = new ArrayList<>();
				if (Boolean.TRUE.equals(claimsVO.getIsExcelExport())) {
					List<ClaimSummaryDetails> claimSummaryDetails = new ArrayList<>();
					for (ClaimSummaryDetails summaryDetail : summaryDetailsList) {
						ClaimSummaryDetails newSummaryDetail = new ClaimSummaryDetails();
						BeanUtils.copyProperties(summaryDetail, newSummaryDetail);
						if (Boolean.FALSE.equals(summaryDetail.getIsExcludedFlag())) {
							claimSummaryDetails.add(summaryDetail);
						}
					}
					prepareClaimSummaryDetails(claimSummaryDetails, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
					setgroupedTransactionsDetails(claimSummaryDetails, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
				} else {
					prepareClaimSummaryDetails(summaryDetailsList, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
					setgroupedTransactionsDetails(summaryDetailsList, result, quarterTotalAmount, quarterAdjustedTotal, finalTransactionResult, finalResult);
				}
			});
		});
		return finalResult;
	}

}

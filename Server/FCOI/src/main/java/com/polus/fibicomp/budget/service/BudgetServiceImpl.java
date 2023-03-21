package com.polus.fibicomp.budget.service;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.budget.common.pojo.ValidCeRateType;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.pojo.BudgetTemplate;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.budget.vo.BudgetHeaderDetail;
import com.polus.fibicomp.budget.vo.BudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.BudgetSummary;
import com.polus.fibicomp.budget.vo.BudgetSummaryVO;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.budget.vo.PeriodCost;
import com.polus.fibicomp.budget.vo.SimpleBudgetLineItemVO;
import com.polus.fibicomp.budget.vo.SimpleBudgetVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.service.ProposalCopyService;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "budgetService")
public class BudgetServiceImpl implements BudgetService {


	protected static Logger logger = LogManager.getLogger(BudgetServiceImpl.class.getName());

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	ProposalCopyService proposalCopyService;

	@Autowired
	@Qualifier("budgetCalculationService")
	private BudgetCalculationService budgetCalculationService;

	@Autowired
	@Qualifier("dateTimeService")
	private DateTimeService dateTimeService;

	@Autowired
	@Qualifier(value = "proposalService")
	private ProposalService proposalService;

	@Autowired
	@Qualifier(value = "budgetPersonService")
	private BudgetPersonService budgetPersonService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;
	
	@Autowired
	@Qualifier(value = "commonService")
	private CommonService commonService;

	@Autowired
	private BudgetModularService budgetModularService;

	@Autowired
	private PersonDao personDao;

	@Override
	public String createProposalBudget(BudgetVO vo) {
		Integer proposalId = vo.getProposalId();
		Proposal proposal = proposalDao.fetchProposalById(proposalId);
		Integer budgetVersionNumber = budgetDao.maxBudgetVersionNumberByProposalId(proposalId);
		if (budgetVersionNumber > 0) {
			fetchBudgetParameterValues(vo);
			budgetVersionNumber = budgetVersionNumber + 1;
			budgetDao.updateBudgetLatestVersionFlag(proposalId, budgetVersionNumber);
		} else {
			budgetVersionNumber = budgetVersionNumber + 1;
		}
		BudgetHeader budget = new BudgetHeader();
		if (vo.getBudgetDescription() != null) {
			budget.setComments(vo.getBudgetDescription());
		}
		if (Boolean.TRUE.equals(vo.getIsApprovedBudget())) {
			budget.setIsApprovedBudget(true);
		}
		budget.setStartDate(proposal.getStartDate());
		budget.setEndDate(proposal.getEndDate());
		budget.setCreateTimeStamp(commonDao.getCurrentTimestamp());
		budget.setCreateUser(vo.getUserName());
		budget.setCreateUserName(vo.getUserFullName());
		budget.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budget.setUpdateUser(vo.getUserName());
		budget.setUpdateUserName(vo.getUserFullName());
		budget.setBudgetStatusCode(Constants.BUDGET_STATUS_INPROGRESS);
		budget.setBudgetStatus(budgetDao.getBudgetStatusById(Constants.BUDGET_STATUS_INPROGRESS));
		budget.setVersionNumber(budgetVersionNumber);
		budget.setTotalCost(BigDecimal.ZERO);
		budget.setTotalCostLimit(BigDecimal.ZERO);
		budget.setTotalDirectCost(BigDecimal.ZERO);
		budget.setTotalIndirectCost(BigDecimal.ZERO);
		budget.setTotalSubcontractCost(BigDecimal.ZERO);
		budget.setUnderrecoveryAmount(BigDecimal.ZERO);
		budget.setCostSharingAmount(BigDecimal.ZERO);
		String rateClassCode = null;
		String rateTypeCode = null;
		if (proposal.getGrantCallId() != null) {
			GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
			rateClassCode = grantCall.getRateClassCode() != null ? grantCall.getRateClassCode() : commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_CLASS_CODE);
			rateTypeCode = grantCall.getRateTypeCode() != null ? grantCall.getRateTypeCode() : commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_TYPE_CODE);
		} else {
			rateClassCode = commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_CLASS_CODE);
			rateTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_TYPE_CODE);
		}
		budget.setRateClassCode(rateClassCode);
		budget.setRateTypeCode(rateTypeCode);
		budget.setRateType(budgetDao.getOHRateTypeByParams(rateClassCode, rateTypeCode));
		budget.setUnderrecoveryRateClassCode(rateClassCode);
		budget.setUnderrecoveryRateTypeCode(rateTypeCode);
		budget.setUnderrecoveryRateType(budgetDao.getOHRateTypeByParams(rateClassCode, rateTypeCode));
		if (budget.getStartDate() != null) {
			if (isPeriodGenerationBasedOnCalenderYear()) {
				budget.setBudgetPeriods(generateBudgetPeriodsByCalendarYear(budget));
			} else {
				budget.setBudgetPeriods(generateBudgetPeriods(budget));
			}
		}
		budget.setIsAutoCalc(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AUTO_CALCULATE) ? commonDao.getParameterValueAsBoolean(Constants.DEFAULT_BUDGET_IS_AUTO_CALCULATION) : Boolean.FALSE);
		budget.setProposalId(vo.getProposalId());
		budget.setCampusFlag(Constants.NO);
		budget.setIsSelected(true);
		Set<String> rateClassTypes = new HashSet<>();
		Integer grantTypeCode = proposal.getGrantTypeCode();
		List<FibiProposalRate> fibiProposalRates = fetchFilteredProposalRates(budget, proposal.getActivityTypeCode(), rateClassTypes, grantTypeCode);
		budget.setProposalRates(fibiProposalRates);
		vo.setRateClassTypes(rateClassTypes);
		boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
		vo.setEnableCostShareStatus(isEnableCostShareStatus);
		if (isEnableCostShareStatus) {
			 vo.setCostSharingType(budgetDao.getCostSharingType());
		}
		budget.setIsLatestVersion(true);
		budget = budgetDao.saveBudgetHeader(budget);
		loadBudgetInitialData(vo);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budgetHeader : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budgetHeader));
		}
		vo.setIsBudgetHeaderFound(true);
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setBudgetHeader(budget);
		vo.setGrantTypeCode(grantTypeCode);
		if (proposal.getGrantCallType() != null) {
		vo.setCategoryCode(proposal.getGrantCallType().getCategoryCode());
		vo.setProposalType(proposal.getGrantCallType().getDescription());
		}
		fetchBudgetParameterValues(vo);
		if (commonDao.getParameterValueAsBoolean(Constants.COST_ELEMENT_FROM_TEMPLATE)) {
			vo.setBudgetTemplateTypes(budgetDao.getBudgetTemplateTypesByModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void addTemplateCostElementForProposal(boolean isSimpleBudgetEnabled, BudgetHeader budget, String activityTypeCode, String updateUser, Integer budgetTemplateTypeId) {
		List<BudgetPeriod> budgetPeriods = budget.getBudgetPeriods();
		if (isSimpleBudgetEnabled) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				saveTemplateCostElement(budgetPeriod, activityTypeCode, updateUser, budgetTemplateTypeId);
			}
		} else {
			if (budgetPeriods !=null && !budgetPeriods.isEmpty()) {
				resetTheBudgetDetails(budgetPeriods);
				saveTemplateCostElement(budgetPeriods.get(0), activityTypeCode, updateUser, budgetTemplateTypeId);
			}
		}
	}

	private void resetTheBudgetDetails(List<BudgetPeriod> budgetPeriods) {
		budgetPeriods.stream().forEach(budgetPeriod -> {
			budgetDao.deleteAllBudgetDetail(budgetPeriod.getBudgetDetails());
			budgetPeriod.getBudgetDetails().clear();
			budgetPeriod.getBudgetDetails().addAll(new ArrayList<>());
		});
	}

	private void saveTemplateCostElement(BudgetPeriod budgetPeriod, String activityTypeCode, String updateUser, Integer budgetTemplateTypeId) {
		List<BudgetDetail> newBudgetDetails = new ArrayList<>();
		List<BudgetDetail> cannedBudgetDetails = new ArrayList<>();
		List<BudgetTemplate> budgetTemplates = budgetDao.fetchBudgetTemplatesByTemplateType(budgetTemplateTypeId);
		Integer lineItemNumber = 1;
		for (BudgetTemplate budgetTemplate : budgetTemplates) {
			CostElement costElement = awardBudgetDao.fetchCostElementsById(budgetTemplate.getCostElement());
			if (costElement.isActive()) {
				BudgetDetail budgetDetail = new BudgetDetail();
				budgetDetail.setBudgetPeriod(budgetPeriod.getBudgetPeriod());
				budgetDetail.setPeriod(budgetPeriod);
				budgetDetail.setVersionNumber(budgetPeriod.getVersionNumber());
				budgetDetail.setLineItemNumber(lineItemNumber);
				budgetDetail.setBudgetCategoryCode(costElement.getBudgetCategoryCode());
				budgetDetail.setBudgetCategory(budgetDao.fetchBudgetCategoryBasedOnCode(costElement.getBudgetCategoryCode()));
				budgetDetail.setCostElement(costElement);
				budgetDetail.setCostElementCode(costElement.getCostElement());
				budgetDetail.setLineItemCost(BigDecimal.ZERO);
				budgetDetail.setIsSystemGeneratedCostElement(budgetTemplate.getIsSystemGeneratedCostElement());
				budgetDetail.setSystemGeneratedCEType(budgetTemplate.getSystemGeneratedCEType());
				String lineItemDescription = "Default Item " + lineItemNumber;
				budgetDetail.setLineItemDescription(lineItemDescription);
				budgetDetail.setUpdateUser(updateUser);
				budgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				newBudgetDetails.add(budgetDetail);
				lineItemNumber++;
			}
		}
		/* BUG Fix: Condition - Canned cost element flag = Y
		 *  System Generated cost element flag = N, sysGen CE was pushing into period.
		 *  Solution : is SysGen CE Parameter flag check done
		 */ 
		if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT)) {
			List<CostElement> sysGeneratedCostElements = fetchSysGeneratedCostElements(activityTypeCode);
			for (CostElement costElements : sysGeneratedCostElements) {
				newBudgetDetails.add(setBudgetDetailData(costElements, newBudgetDetails.get(newBudgetDetails.size() - 1), lineItemNumber));
				lineItemNumber++;
			}
		}
		for (BudgetDetail proposalBudgetDetail : newBudgetDetails) {
			budgetDao.saveBudgetDetail(proposalBudgetDetail);
			cannedBudgetDetails.add(proposalBudgetDetail);
		}
		budgetPeriod.getBudgetDetails().clear();
		budgetPeriod.getBudgetDetails().addAll(cannedBudgetDetails);
	}

	private BudgetDetail setBudgetDetailData(CostElement costElement, BudgetDetail budgetDetail, Integer lineItemNumber) {
		BudgetDetail proposalBudgetDetail =new BudgetDetail();
		proposalBudgetDetail.setBudgetPeriod(budgetDetail.getBudgetPeriod());
		proposalBudgetDetail.setVersionNumber(budgetDetail.getVersionNumber());
		proposalBudgetDetail.setPeriod(budgetDetail.getPeriod());
		proposalBudgetDetail.setLineItemNumber(lineItemNumber);
		proposalBudgetDetail.setBudgetCategoryCode(costElement.getBudgetCategoryCode());
		proposalBudgetDetail.setBudgetCategory(costElement.getBudgetCategory());
		proposalBudgetDetail.setCostElement(costElement);
		proposalBudgetDetail.setCostElementCode(costElement.getCostElement());
		proposalBudgetDetail.setLineItemCost(BigDecimal.ZERO);
		proposalBudgetDetail.setIsSystemGeneratedCostElement(true);
		proposalBudgetDetail.setSystemGeneratedCEType(costElement.getSystemGeneratedCEType());
		proposalBudgetDetail.setUpdateUser(budgetDetail.getUpdateUser());
		proposalBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		return proposalBudgetDetail;
	}

	@Override
	public void loadBudgetInitialData(BudgetVO vo) {
		vo.setIsCalculationWithPredefinedSalary(commonDao.getParameterValueAsBoolean(Constants.IS_CALCULATION_WITH_PREDEFINED_SALARY));
		String activityTypeCode = vo.getActivityTypeCode();
		if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT)) {
			vo.setSysGeneratedCostElements(fetchSysGeneratedCostElements(activityTypeCode));
		}
		vo.setBudgetStatus(budgetDao.fetchAllBudgetStatus());
		String ohRateClassTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE);
		vo.setRateTypes(budgetDao.fetchRateTypeByParams("1", budgetDao.fetchRateClassCodesByType(ohRateClassTypeCode)));
	}

	@Override
	public String saveOrUpdateProposalBudget(BudgetVO vo) {
		Integer proposalId = vo.getProposalId();
		Integer budgetPeriod = vo.getBudgetPeriod();
		Integer previousFinalBudgetId = vo.getPreviousFinalBudgetId();
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		String budgetTabName = vo.getBudgetTabName(); // DETAILED, SIMPLE, CATEGORY_TOTAL, BUDGET_SUMMARY
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod period : budgetPeriods) {
				List<BudgetDetail> budgetDetail = period.getBudgetDetails();
				if (budgetDetail != null && !budgetDetail.isEmpty()) {
					for (BudgetDetail detail : budgetDetail) {
						if (budgetTabName != null && budgetTabName.equals(Constants.BUDGET_TAB_SIMPLE) && detail.getSponsorRequestedAmount() != null) {
							detail.setLineItemCost(detail.getSponsorRequestedAmount());
						}
						List<BudgetPersonalDetails> budgetPersonalDetails = detail.getPersonsDetails();
						if (budgetPersonalDetails != null && !budgetPersonalDetails.isEmpty()) {
							for (BudgetPersonalDetails personalDetail : budgetPersonalDetails) {
								if (personalDetail.getBudgetPerson() != null) {
									personalDetail.setBudgetPersonId(personalDetail.getBudgetPerson().getBudgetPersonId());
									personalDetail.setRolodexId(personalDetail.getBudgetPerson().getRolodexId());
									personalDetail.setPersonId(personalDetail.getBudgetPerson().getPersonId());
									personalDetail.setPersonType(personalDetail.getBudgetPerson().getPersonType());
									personalDetail.setPersonName(personalDetail.getBudgetPerson().getPersonName());
									if (personalDetail.getBudgetPerson().getPersonType().equals(Constants.TBN_PERSON_TYPE)) {
										personalDetail.setTbnId(personalDetail.getBudgetPerson().getTbnId());
										personalDetail.setPersonName(personalDetail.getBudgetPerson().getTbnPerson().getPersonName());
									}
								}
							}
						}
					}
				}
			}
		}
		budgetHeader.setUpdateUserName(personDao.getPersonFullNameByPersonId(AuthenticatedUser.getLoginPersonId()));
		budgetDao.saveBudgetHeader(budgetHeader);
		if (previousFinalBudgetId != null) {
			BudgetHeader budget = budgetDao.fetchBudgetByBudgetId(previousFinalBudgetId);
			budget.setIsFinalBudget(false);
			budgetDao.saveBudgetHeader(budget);
		}
		if (vo.getBudgetTemplateTypeId() != null) {
			addTemplateCostElementForProposal(vo.getIsSimpleBudgetEnabled(), budgetHeader, vo.getActivityTypeCode(), vo.getUpdateUser(), vo.getBudgetTemplateTypeId());
		}
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			calculate(budgetHeader, budgetPeriod, vo.getActivityTypeCode());
		} else {
			calculateByPeriodCost(budgetHeader);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		boolean isSimpleBudget = commonDao.getParameterValueAsBoolean(Constants.IS_SIMPLE_BUDGET_ENABLED);
		if (isSimpleBudget) {
			vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		}
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setGrantTypeCode(vo.getGrantTypeCode());
		vo.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(vo.getGrantTypeCode()));
		return commonDao.convertObjectToJSON(vo);
	}
	
    // calculate period costs when auto calc is off on saveORUpdateBudget call
	private void calculateByPeriodCost(BudgetHeader budgetHeader) {
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
					if (budgetPeriod != null && budgetPeriod.getBudgetDetails().isEmpty() && Boolean.TRUE.equals(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND)) && Boolean.FALSE.equals(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_COST_SHARE_AND_UNDERRECOVERY))) {
						budgetPeriod.setCostSharingAmount(budgetPeriod.getTotalInKind());
					}	
					BigDecimal totalFringeCostLineItem = BigDecimal.ZERO;
					BigDecimal totalFringeCostFundRequested = BigDecimal.ZERO;
					BigDecimal totalFandACostLineItem = BigDecimal.ZERO;
					BigDecimal totalFandACostFundRequested = BigDecimal.ZERO;
					BigDecimal totalFringeCostCS = BigDecimal.ZERO;
					BigDecimal totalFandACostCS = BigDecimal.ZERO;
					BigDecimal totalLineItemCost = BigDecimal.ZERO;
					BigDecimal totalFundRequested = BigDecimal.ZERO;
					BigDecimal totalCostSharing = BigDecimal.ZERO;
					BigDecimal totalUnderRecovery = BigDecimal.ZERO;
					BigDecimal underRecovery = BigDecimal.ZERO;
					List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
					if (budgetDetails != null && !budgetDetails.isEmpty()) {
						for (BudgetDetail budgetItemDetail : budgetDetails) {
							if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
								totalLineItemCost = totalLineItemCost.add(budgetItemDetail.getLineItemCost());
								if (budgetItemDetail.getSponsorRequestedAmount() != null) {
									totalFundRequested = totalFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
								}
								if (budgetItemDetail.getCostSharingAmount() != null) {
									totalCostSharing = totalCostSharing.add(budgetItemDetail.getCostSharingAmount());
								}
							} else {
								if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
									if (budgetItemDetail.getSponsorRequestedAmount() != null) {
										totalFringeCostFundRequested = totalFringeCostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
									}
									if (budgetItemDetail.getCostSharingAmount() != null) {
										totalFringeCostCS = totalFringeCostCS.add(budgetItemDetail.getCostSharingAmount());
									}
									if (budgetItemDetail.getLineItemCost() != null) {
										totalFringeCostLineItem = totalFringeCostLineItem.add(budgetItemDetail.getLineItemCost());
									}
								}
								if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
									if (budgetItemDetail.getSponsorRequestedAmount() != null) {
										totalFandACostFundRequested = totalFandACostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
									}
									if (budgetItemDetail.getCostSharingAmount() != null) {
										totalFandACostCS = totalFandACostCS.add(budgetItemDetail.getCostSharingAmount());
									}
									if (budgetItemDetail.getLineItemCost() != null) {
										totalFandACostLineItem = totalFandACostLineItem.add(budgetItemDetail.getLineItemCost());
									}
								}
								if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_RESEARCH_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
									if (budgetItemDetail.getSponsorRequestedAmount() != null) {
										totalFandACostFundRequested = totalFandACostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
									}
									if (budgetItemDetail.getCostSharingAmount() != null) {
										totalFandACostCS = totalFandACostCS.add(budgetItemDetail.getCostSharingAmount());
									}
									if (budgetItemDetail.getLineItemCost() != null) {
										totalFandACostLineItem = totalFandACostLineItem.add(budgetItemDetail.getLineItemCost());
									}
								}
							}
						}
						budgetPeriod.setTotalDirectCost(totalFundRequested.add(totalFringeCostFundRequested).setScale(2, RoundingMode.HALF_UP));
						budgetPeriod.setTotalIndirectCost(totalFandACostFundRequested.setScale(2, RoundingMode.HALF_UP));
						totalCostSharing = totalCostSharing.add(totalFandACostCS).add(totalFringeCostCS);
						budgetPeriod.setCostSharingAmount(totalCostSharing.setScale(2, RoundingMode.HALF_UP));
						totalUnderRecovery = totalUnderRecovery.add(underRecovery);
						budgetPeriod.setUnderrecoveryAmount(totalUnderRecovery);
						budgetPeriod.setTotalInKind(budgetPeriod.getTotalInKind().setScale(2, RoundingMode.HALF_UP));
						budgetPeriod.setTotalOfTotalCost(budgetPeriod.getTotalOfTotalCost().setScale(2, RoundingMode.HALF_UP));
						budgetPeriod.setTotalModifiedDirectCost(budgetPeriod.getTotalModifiedDirectCost().setScale(2, RoundingMode.HALF_UP));
						budgetPeriod.setTotalCost(budgetPeriod.getTotalDirectCost().add(budgetPeriod.getTotalIndirectCost()).setScale(2, RoundingMode.HALF_UP));
					}
				}
			updateBudgetHeader(budgetHeader);
	}

	public BudgetHeader calculateBudgetDetails(BudgetHeader budgetHeader, String activityTypeCode) {
		calculate(budgetHeader, null, activityTypeCode);
		return budgetHeader;
	}

	private void calculate(BudgetHeader budgetHeader, Integer period, String activityTypeCode) {
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			calculationOnPeriodBased(budgetHeader, budgetPeriod, activityTypeCode);
		}
		updateBudgetHeader(budgetHeader);
	}

	private void calculationOnPeriodBased(BudgetHeader budgetHeader, BudgetPeriod budgetPeriod,String activityTypeCode) {
		BigDecimal totalFringeCost = BigDecimal.ZERO;
		BigDecimal totalFringeCostCS = BigDecimal.ZERO;
		BigDecimal totalFringeCostForFundRequested = BigDecimal.ZERO;
		BigDecimal totalFandACost = BigDecimal.ZERO;
		BigDecimal totalFandACostCS = BigDecimal.ZERO;
		BigDecimal totalFandACostForFundRequested = BigDecimal.ZERO;
		BigDecimal totalLineItemCost = BigDecimal.ZERO;
		BigDecimal totalCostSharingAmount = BigDecimal.ZERO;
		BigDecimal totalFundRequested = BigDecimal.ZERO;
		BigDecimal totalUnderrecoveryAmount = BigDecimal.ZERO;
		BigDecimal fundRequested = BigDecimal.ZERO;
		BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
		if (budgetPeriod != null) {
		List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			for (BudgetDetail budgetItemDetail : budgetDetails) {
				if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
					List<BudgetDetailCalcAmount> budgetDetailCalcAmounts = budgetItemDetail.getBudgetDetailCalcAmounts();
					List<BudgetDetailCalcAmount> updatedbudgetDetailCalcAmounts = new ArrayList<BudgetDetailCalcAmount>(budgetDetailCalcAmounts);
					Collections.copy(updatedbudgetDetailCalcAmounts, budgetDetailCalcAmounts);
					for (BudgetDetailCalcAmount budgetDetailCalAmt : budgetDetailCalcAmounts) {
						if (!budgetDetailCalAmt.getRateClassCode().equals(Constants.RATE_CLASS_CODE_INFLATION)) {
							updatedbudgetDetailCalcAmounts.remove(budgetDetailCalAmt);
							budgetDao.deleteBudgetDetailCalcAmount(budgetDetailCalAmt);
						}
					}
					budgetItemDetail.getBudgetDetailCalcAmounts().clear();
					budgetItemDetail.getBudgetDetailCalcAmounts().addAll(updatedbudgetDetailCalcAmounts);
					
					BigDecimal fandACostForCE = BigDecimal.ZERO;
					BigDecimal fringeCostForCE = BigDecimal.ZERO;
					BigDecimal fandACostSharing = BigDecimal.ZERO;
					BigDecimal fandAFundRequested = BigDecimal.ZERO;
					BigDecimal fringeCostForCostSharing = BigDecimal.ZERO;
					BigDecimal fringeCostForFundRequested = BigDecimal.ZERO;
					BigDecimal costShareAmount = budgetItemDetail.getCostSharingAmount();
					BigDecimal lineItemCost = budgetItemDetail.getLineItemCost();
					BigDecimal fundRequestedCost = budgetItemDetail.getSponsorRequestedAmount();
					
					fringeCostForCE = calculateFringeCostForCE(budgetHeader, budgetPeriod, budgetItemDetail, lineItemCost, activityTypeCode, Constants.COST_TYPE_LINE_ITEM_COST);					
					fringeCostForCostSharing = calculateFringeCostForCE(budgetHeader, budgetPeriod, budgetItemDetail, costShareAmount, activityTypeCode, Constants.COST_TYPE_COST_SHARE);
					fringeCostForFundRequested = calculateFringeCostForCE(budgetHeader, budgetPeriod, budgetItemDetail, fundRequestedCost, activityTypeCode, Constants.COST_TYPE_FUND_REQUESTED);
					
					fandACostForCE = calculateFandACostForCE(budgetHeader, budgetPeriod, budgetItemDetail, baseCostForCalculatingFandA(budgetItemDetail, fringeCostForCE, Constants.COST_TYPE_LINE_ITEM_COST), activityTypeCode, Constants.COST_TYPE_LINE_ITEM_COST);	
					fandACostSharing = calculateFandACostForCE(budgetHeader, budgetPeriod, budgetItemDetail, baseCostForCalculatingFandA(budgetItemDetail, fringeCostForCostSharing, Constants.COST_TYPE_COST_SHARE), activityTypeCode, Constants.COST_TYPE_COST_SHARE);	
					fandAFundRequested = calculateFandACostForCE(budgetHeader, budgetPeriod, budgetItemDetail, baseCostForCalculatingFandA(budgetItemDetail, fringeCostForFundRequested, Constants.COST_TYPE_FUND_REQUESTED), activityTypeCode, Constants.COST_TYPE_FUND_REQUESTED);	
					
					totalLineItemCost = totalLineItemCost.add(lineItemCost);
					if (budgetItemDetail.getSponsorRequestedAmount() != null) {
						fundRequested = budgetItemDetail.getSponsorRequestedAmount();
						totalFundRequested = totalFundRequested.add(fundRequested);
					}
					budgetItemDetail.setCostSharingAmount(costShareAmount.setScale(2, RoundingMode.HALF_UP));
					totalFandACost = totalFandACost.add(fandACostForCE);
					totalFringeCost = totalFringeCost.add(fringeCostForCE);
					totalFringeCostCS = totalFringeCostCS.add(fringeCostForCostSharing);
					totalFandACostCS = totalFandACostCS.add(fandACostSharing);
					totalFringeCostForFundRequested = totalFringeCostForFundRequested.add(fringeCostForFundRequested);
					totalFandACostForFundRequested = totalFandACostForFundRequested.add(fandAFundRequested);
					
					//Underrecovery for a line item
					BigDecimal underRecoveryAmount = BigDecimal.ZERO;
					Boolean isShowInKind = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND);
					Boolean isShowCostShareUnderRecovery = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_COST_SHARE_AND_UNDERRECOVERY);
					if (Boolean.FALSE.equals(budgetItemDetail.getIsSystemGeneratedCostElement()) && (Boolean.TRUE.equals(isShowInKind) || Boolean.TRUE.equals(isShowCostShareUnderRecovery))) {
						if (!budgetItemDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_EQUIPMENT) && !budgetItemDetail.getBudgetCategory().getBudgetCategoryTypeCode() .equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SUBCONTRACT)) {
							underRecoveryAmount = calculateUnderRecoveryAmountForCE(budgetHeader, budgetPeriod, budgetItemDetail, budgetItemDetail.getCalculateFundRequestedIncludingBenifit(), activityTypeCode, fandAFundRequested);
							budgetItemDetail.setUnderrecoveryAmount(underRecoveryAmount);
							} else {
								budgetItemDetail.setUnderrecoveryAmount(underRecoveryAmount);
							}
					} else {
						budgetItemDetail.setUnderrecoveryAmount(underRecoveryAmount);
					}
					totalUnderrecoveryAmount = totalUnderrecoveryAmount.add(budgetItemDetail.getUnderrecoveryAmount());
					totalCostSharingAmount = totalCostSharingAmount.add(costShareAmount);
				}
			}
			for (BudgetDetail budgetItemDetail : budgetDetails) {
				if (Boolean.TRUE.equals(budgetItemDetail.getIsSystemGeneratedCostElement())) {
					List<BudgetDetailCalcAmount> budgetDetailCalcAmounts = budgetItemDetail.getBudgetDetailCalcAmounts();
					List<BudgetDetailCalcAmount> updatedBudgetDetailCalcAmounts = new ArrayList<BudgetDetailCalcAmount>(budgetDetailCalcAmounts);
					Collections.copy(updatedBudgetDetailCalcAmounts, budgetDetailCalcAmounts);
					for (BudgetDetailCalcAmount amount : budgetDetailCalcAmounts) {
						if (!amount.getRateClassCode().equals(Constants.RATE_CLASS_CODE_INFLATION)) {
							updatedBudgetDetailCalcAmounts.remove(amount);
						}
					}
					budgetItemDetail.getBudgetDetailCalcAmounts().clear();
					budgetItemDetail.getBudgetDetailCalcAmounts().addAll(updatedBudgetDetailCalcAmounts);

					if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						budgetItemDetail.setLineItemCost(totalFringeCost.setScale(2, RoundingMode.HALF_UP));
						budgetItemDetail.setTotalFundRequested(fundRequested.setScale(2, RoundingMode.HALF_UP));
						budgetItemDetail.setCostSharingAmount(totalFringeCostCS.setScale(2, RoundingMode.HALF_UP));
						budgetItemDetail.setSponsorRequestedAmount(totalFringeCostForFundRequested.setScale(2, RoundingMode.HALF_UP));
					}
					if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						totalFandACostCS = totalFandACostCS.add(totalUnderrecoveryAmount);
						budgetItemDetail.setLineItemCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
						budgetItemDetail.setCostSharingAmount(totalFandACostCS.setScale(2, RoundingMode.HALF_UP));
						budgetItemDetail.setSponsorRequestedAmount(totalFandACostForFundRequested.setScale(2, RoundingMode.HALF_UP));
					}
					if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						budgetItemDetail.setLineItemCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
					}
					//it is set null on auto calc on for sys gen costs
					budgetItemDetail.setCostSharingPercentage(null);
				}
			}
		}
		budgetPeriod.setUnderrecoveryAmount(totalUnderrecoveryAmount.setScale(2, RoundingMode.HALF_UP));
		budgetPeriod.setTotalDirectCost(totalFringeCostForFundRequested.add(totalFundRequested).setScale(2, RoundingMode.HALF_UP));
		budgetPeriod.setTotalIndirectCost((totalFandACostForFundRequested).setScale(2, RoundingMode.HALF_UP));
		budgetPeriod.setCostSharingAmount(totalCostSharingAmount.add(totalFringeCostCS).add(totalFandACostCS).setScale(2, RoundingMode.HALF_UP));
		budgetPeriod.setTotalCost((budgetPeriod.getTotalDirectCost()).add(budgetPeriod.getTotalIndirectCost()).setScale(2, RoundingMode.HALF_UP));
		if (budgetDetails != null && budgetDetails.isEmpty()) {
			budgetPeriod.setTotalModifiedDirectCost(totalModifiedDirectCost.setScale(2, RoundingMode.HALF_UP));
		}
	 }
	}

	private BigDecimal baseCostForCalculatingFandA(BudgetDetail budgetItemDetail, BigDecimal fringeCostForCE, String costType) {
		BigDecimal calculationBaseAmount = BigDecimal.ZERO;
		BigDecimal cost = BigDecimal.ZERO;
		if (costType.equalsIgnoreCase(Constants.COST_TYPE_LINE_ITEM_COST)) {
		 cost = budgetItemDetail.getLineItemCost();
		} else if (costType.equalsIgnoreCase(Constants.COST_TYPE_COST_SHARE)) {
			 cost = budgetItemDetail.getCostSharingAmount();
		} else {
			 cost = budgetItemDetail.getSponsorRequestedAmount();
		}
		if (Constants.BUDGET_CATEGORY_CODE_TYPE_PERSONNEL.equals(budgetItemDetail.getBudgetCategory().getBudgetCategoryTypeCode())) {	
			calculationBaseAmount = fringeCostForCE.add(cost);
		} else {
			calculationBaseAmount = cost;
		}
		return calculationBaseAmount;
	}
   
	//Used to update budget header fields for all periods
	private void updateBudgetHeader(BudgetHeader budget) {
		List<BudgetPeriod> budgetPeriods = budget.getBudgetPeriods();
		BigDecimal totalDirectCost = BigDecimal.ZERO;
		BigDecimal totalIndirectCost = BigDecimal.ZERO;
		BigDecimal totalCost = BigDecimal.ZERO;
		BigDecimal totalSubcontractCost = BigDecimal.ZERO;
		BigDecimal totalCostSharingAmount = BigDecimal.ZERO;
		BigDecimal totalUnderrecoveryAmount = BigDecimal.ZERO;
		BigDecimal totalFundRequestedAmount = BigDecimal.ZERO;
		BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
		BigDecimal totalInKind = BigDecimal.ZERO;
		BigDecimal totalOfTotalCost = BigDecimal.ZERO;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod period : budgetPeriods) {
				if (period.getTotalDirectCost() != null) {
					totalDirectCost = totalDirectCost.add(period.getTotalDirectCost());
				}
				if (period.getTotalIndirectCost() != null) {
					totalIndirectCost = totalIndirectCost.add(period.getTotalIndirectCost());
				}
				if (period.getTotalCost() != null) {
					totalCost = totalCost.add(period.getTotalCost());
				}
				if (period.getSubcontractCost() != null) {
					totalSubcontractCost = totalSubcontractCost.add(period.getSubcontractCost());
				}
				if (period.getCostSharingAmount() != null) {
					totalCostSharingAmount = totalCostSharingAmount.add(period.getCostSharingAmount());
				}
				if (period.getUnderrecoveryAmount() != null) {
					totalUnderrecoveryAmount = totalUnderrecoveryAmount.add(period.getUnderrecoveryAmount());
				}
				if (period.getTotalFundRequestedIncludBenifit() != null) {
					totalFundRequestedAmount = totalFundRequestedAmount.add(period.getTotalFundRequestedIncludBenifit());
				}
				if (period.getTotalModifiedDirectCost() != null) {
					totalModifiedDirectCost = totalModifiedDirectCost.add(period.getTotalModifiedDirectCost());
				}
				if (period.getTotalInKind() != null) {
					totalInKind = totalInKind.add(period.getTotalInKind());
				}
				if (period.getTotalOfTotalCost() != null) {
					totalOfTotalCost = totalOfTotalCost.add(period.getTotalOfTotalCost());
				}
			}
		}
		budget.setTotalDirectCost(totalDirectCost.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalIndirectCost(totalIndirectCost.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalCost(totalCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
		budget.setTotalSubcontractCost(totalSubcontractCost.setScale(2, RoundingMode.HALF_UP));
		budget.setUnderrecoveryAmount(totalUnderrecoveryAmount.setScale(2, RoundingMode.HALF_UP));
		budget.setCostSharingAmount(totalCostSharingAmount.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalFundRequested(totalFundRequestedAmount.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalModifiedDirectCost(totalModifiedDirectCost.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalInKind(totalInKind.setScale(2, RoundingMode.HALF_UP));
		budget.setTotalOfTotalCost(totalOfTotalCost.setScale(2, RoundingMode.HALF_UP));
	}
	
	private BigDecimal calculateFringeCostForCE(BudgetHeader budgetHeader, BudgetPeriod budgetPeriod, BudgetDetail budgetDetail, BigDecimal cost, String activityTypeCode, String costType) {
		BigDecimal fringeCost = BigDecimal.ZERO;
		Integer budgetId = budgetHeader.getBudgetId();
		CostElement costElement = budgetDetail.getCostElement();
		costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
		BudgetDetailCalcAmount budgetCalculatedAmount = null;
		if (costType.equalsIgnoreCase(Constants.COST_TYPE_LINE_ITEM_COST)) {
		List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
		if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
			for (ValidCeRateType ceRateType : ceRateTypes) {
				FibiProposalRate proposalRates = budgetDao.fetchApplicableProposalRate(budgetId, budgetPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), activityTypeCode);
				if (proposalRates != null && (proposalRates.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_EMPLOYEE_BENEFITS) && Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS.equals(proposalRates.getRateClassCode()))) {
					BigDecimal validApplicableRate = BigDecimal.ZERO;
					validApplicableRate = validApplicableRate.add(proposalRates.getApplicableRate());
					BigDecimal instituteRate = BigDecimal.ZERO;
					instituteRate = instituteRate.add(proposalRates.getInstituteRate());
					if (validApplicableRate.compareTo(BigDecimal.ZERO) > 0) {
						BigDecimal hundred = new BigDecimal(100);
						BigDecimal applicablePercentageFactor = validApplicableRate.divide(hundred);
						BigDecimal calculatedCost = cost.multiply(applicablePercentageFactor).setScale(2, RoundingMode.HALF_UP);
						fringeCost = fringeCost.add(calculatedCost);
						budgetCalculatedAmount = getNewBudgetCalculatedAmount(budgetPeriod, budgetDetail, proposalRates);
						budgetCalculatedAmount.setCalculatedCost(calculatedCost);
						budgetDetail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
					}
				}
			}
		}
		} else {
			if (budgetDetail.getBudgetDetailCalcAmounts() != null && !budgetDetail.getBudgetDetailCalcAmounts().isEmpty()) {
				for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetail.getBudgetDetailCalcAmounts()) {
					BigDecimal hundred = new BigDecimal(100);
					if (budgetDetailCalcAmount.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_EMPLOYEE_BENEFITS) || ((budgetDetailCalcAmount.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_INFLATION) && !budgetDetail.getBudgetPeriod().equals(1)))) {
						BigDecimal applicablePercentageFactor = budgetDetailCalcAmount.getApplicableRate().divide(hundred);
						BigDecimal calculatedCost = cost.multiply(applicablePercentageFactor).setScale(2, RoundingMode.HALF_UP);
						fringeCost = fringeCost.add(calculatedCost);
						if (costType.equalsIgnoreCase(Constants.COST_TYPE_COST_SHARE)) {
							budgetDetailCalcAmount.setCalculatedCostSharing(calculatedCost);
						} else {
							budgetDetailCalcAmount.setCalculatedFundRequested(calculatedCost);
						}
						if (budgetDetailCalcAmount.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_INFLATION)) {
							fringeCost = BigDecimal.ZERO;
						}
					}
				}
			}
		}
		return fringeCost;
	}

	private BigDecimal calculateFandACostForCE(BudgetHeader budgetHeader, BudgetPeriod budgetPeriod, BudgetDetail budgetDetail, BigDecimal costWithBenefit, String activityTypeCode, String costType) {
		BigDecimal fandACost = BigDecimal.ZERO;
		Integer budgetId = budgetHeader.getBudgetId();
		CostElement costElement = budgetDetail.getCostElement();
		costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
		// Rounding mode is used to remove an exception thrown in BigDecimal division to get rounding up to 2 precision);
		BudgetDetailCalcAmount budgetCalculatedAmount = null;
		if (costType.equalsIgnoreCase(Constants.COST_TYPE_LINE_ITEM_COST)) {
		List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
		if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
			for (ValidCeRateType ceRateType : ceRateTypes) {
				if (ceRateType.getRateClassCode().equals(budgetHeader.getRateClassCode()) && ceRateType.getRateTypeCode().equalsIgnoreCase(budgetHeader.getRateTypeCode())) {
					FibiProposalRate proposalRates = budgetDao.fetchApplicableProposalRate(budgetId, budgetPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), activityTypeCode);
					if (proposalRates != null) {
						BigDecimal validApplicableRate = BigDecimal.ZERO;
						validApplicableRate = validApplicableRate.add(proposalRates.getApplicableRate());
						BigDecimal instituteRate = BigDecimal.ZERO;
						instituteRate= instituteRate.add(proposalRates.getInstituteRate());
						if (validApplicableRate.compareTo(BigDecimal.ZERO) > 0 || instituteRate.compareTo(BigDecimal.ZERO) > 0) {
							BigDecimal hundred = new BigDecimal(100);
							BigDecimal applicablePercentageFactor = BigDecimal.ZERO;
							if (validApplicableRate.compareTo(BigDecimal.ZERO) > 0) {
								applicablePercentageFactor = validApplicableRate.divide(hundred);
							} else {
								applicablePercentageFactor = BigDecimal.ZERO;
							}
							BigDecimal instituteRatePercentageFactor = instituteRate.divide(hundred);
							BigDecimal calculatedCost = BigDecimal.ZERO;
							if (costType.equalsIgnoreCase(Constants.COST_TYPE_FUND_REQUESTED)) {
								calculatedCost = (costWithBenefit.multiply(applicablePercentageFactor).setScale(2, RoundingMode.HALF_UP));
							} else {
								 calculatedCost = (costWithBenefit.multiply(instituteRatePercentageFactor).setScale(2, RoundingMode.HALF_UP));
							}
							fandACost = fandACost.add(calculatedCost);
							budgetCalculatedAmount = getNewBudgetCalculatedAmount(budgetPeriod, budgetDetail, proposalRates);
							budgetCalculatedAmount.setCalculatedCost(calculatedCost);
							budgetDetail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
						}
					}
				}
			}
		 }
		} else {
			if (budgetDetail.getBudgetDetailCalcAmounts() != null && !budgetDetail.getBudgetDetailCalcAmounts().isEmpty()) {
				for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetail.getBudgetDetailCalcAmounts()) {
					BigDecimal hundred = new BigDecimal(100);
					BigDecimal percentageFactor = BigDecimal.ZERO;
					BigDecimal calculatedCost = BigDecimal.ZERO;
					if (budgetDetailCalcAmount.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_OVERHEAD)) {
						if (costType.equalsIgnoreCase(Constants.COST_TYPE_COST_SHARE)) {
							percentageFactor = budgetDetailCalcAmount.getApplicableCostSharingRate().divide(hundred);
							calculatedCost = costWithBenefit.multiply(percentageFactor).setScale(2, RoundingMode.HALF_UP);
							budgetDetailCalcAmount.setCalculatedCostSharing(calculatedCost);
						} else {
							percentageFactor = budgetDetailCalcAmount.getApplicableRate().divide(hundred);
							calculatedCost = costWithBenefit.multiply(percentageFactor).setScale(2, RoundingMode.HALF_UP);
							budgetDetailCalcAmount.setCalculatedFundRequested(calculatedCost);
						}
						fandACost = fandACost.add(calculatedCost);
					}
				}
			}
		}
		return fandACost;
	}

	//calculation of underRecovery amount for cost elements
	private BigDecimal calculateUnderRecoveryAmountForCE(BudgetHeader budgetHeader, BudgetPeriod budgetPeriod, BudgetDetail budgetDetail, BigDecimal costWithBenefit, String activityTypeCode, BigDecimal indirectCostFundRequested) {
		BigDecimal underRecoveryCost = BigDecimal.ZERO;
		Integer budgetId = budgetHeader.getBudgetId();
		CostElement costElement = budgetDetail.getCostElement();
		costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
		List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
		if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
			for (ValidCeRateType ceRateType : ceRateTypes) {
				if (ceRateType.getRateClassCode().equals(budgetHeader.getUnderrecoveryRateClassCode())
						&& ceRateType.getRateTypeCode().equalsIgnoreCase(budgetHeader.getUnderrecoveryRateTypeCode())) {
					FibiProposalRate proposalRates = budgetDao.fetchApplicableProposalRate(budgetId, budgetPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), activityTypeCode);
					if (proposalRates != null && proposalRates.getRateClassCode().equals(budgetHeader.getUnderrecoveryRateClassCode()) && proposalRates.getRateTypeCode().equals(budgetHeader.getUnderrecoveryRateTypeCode())) {
						BigDecimal instituteRate = BigDecimal.ZERO;
						// (COST with benefit * IR) - IC
						instituteRate = instituteRate.add(proposalRates.getInstituteRate());
						BigDecimal hundred = new BigDecimal(100);
						BigDecimal percentageFactor = instituteRate.divide(hundred);
						BigDecimal calculatedCost = new BigDecimal(100);
						calculatedCost = (costWithBenefit.multiply(percentageFactor));
						underRecoveryCost = underRecoveryCost.add(calculatedCost.setScale(2, RoundingMode.HALF_UP));
						underRecoveryCost = underRecoveryCost.subtract(indirectCostFundRequested.setScale(2, RoundingMode.HALF_UP));
					}
				}
			}
		}
		return underRecoveryCost;
	}

	@Override
	public BudgetDetailCalcAmount getNewBudgetCalculatedAmount(BudgetPeriod budgetPeriod, BudgetDetail budgetDetail, FibiProposalRate proposalRate) {
		BudgetDetailCalcAmount budgetCalculatedAmount = new BudgetDetailCalcAmount();
		budgetCalculatedAmount.setBudgetId(budgetPeriod.getBudget().getBudgetId());
		budgetCalculatedAmount.setBudgetPeriod(budgetDetail.getBudgetPeriod());
		budgetCalculatedAmount.setBudgetPeriodId(budgetPeriod.getBudgetPeriodId());
		budgetCalculatedAmount.setLineItemNumber(budgetDetail.getLineItemNumber());
		budgetCalculatedAmount.setRateClassCode(proposalRate.getRateClassCode());
		budgetCalculatedAmount.setRateClass(proposalRate.getRateClass());
		budgetCalculatedAmount.setRateTypeCode(proposalRate.getRateTypeCode());
		budgetCalculatedAmount.setRateType(proposalRate.getRateType());
		budgetCalculatedAmount.setApplyRateFlag(true);
		budgetCalculatedAmount.setRateTypeDescription(proposalRate.getRateType().getDescription());
		budgetCalculatedAmount.setBudgetDetail(budgetDetail);
		budgetCalculatedAmount.setBudgetDetailId(budgetDetail.getBudgetDetailId());
		budgetCalculatedAmount.setApplicableRate(proposalRate.getApplicableRate());
		budgetCalculatedAmount.setApplicableCostSharingRate(proposalRate.getInstituteRate());
		budgetCalculatedAmount.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budgetCalculatedAmount.setUpdateUser(budgetDetail.getUpdateUser());
		return budgetCalculatedAmount;
	}

	@Override
	public String autoCalculate(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		budgetDao.saveBudgetHeader(budgetHeader);
		String activityTypeCode = vo.getActivityTypeCode();
		Integer proposalId = vo.getProposalId();
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			calculate(budgetHeader, null, activityTypeCode);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		String budgetTabName = vo.getBudgetTabName();
		if (budgetTabName.equals(Constants.BUDGET_TAB_SIMPLE)) {
			vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		}
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<BudgetPeriod> generateBudgetPeriods(BudgetHeader budget) {
		List<BudgetPeriod> budgetPeriods = new ArrayList<BudgetPeriod>();
		Date projectStartDate = new Date(budget.getStartDate().getTime());
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;
		Calendar cl = Calendar.getInstance();
		Date periodStartDate = projectStartDate;
		int budgetPeriodNum = 1;
		if (commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_BUDGET_ENABLE_SINGLE_PERIOD)) {
				cl.setTime(periodStartDate);
				cl.add(Calendar.YEAR, 1);
				/* check period end date gt project end date */
				BudgetPeriod budgetPeriod = new BudgetPeriod();
				budgetPeriod.setTotalCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
				budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
				budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
				budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
				budgetPeriod.setBudgetPeriod(budgetPeriodNum);
				Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
				Timestamp periodEndDateTimeStamp = new Timestamp(projectEndDate.getTime());
				budgetPeriod.setStartDate(periodStartDateTimeStamp);
				budgetPeriod.setEndDate(periodEndDateTimeStamp);
				budgetPeriod.setBudget(budget);
				budgetPeriods.add(budgetPeriod);
		} else {
			while (budgetPeriodExists) {
				cl.setTime(periodStartDate);
				cl.add(Calendar.YEAR, 1);
				Date nextPeriodStartDate = new Date(cl.getTime().getTime());
				cl.add(Calendar.DATE, -1);
				Date periodEndDate = new Date(cl.getTime().getTime());
				/* check period end date gt project end date */
				switch (periodEndDate.compareTo(projectEndDate)) {
				case 1:
					periodEndDate = projectEndDate;
					// the break statement is purposefully missing.
				case 0:
					budgetPeriodExists = false;
					break;
				}
				BudgetPeriod budgetPeriod = new BudgetPeriod();
				budgetPeriod.setTotalCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
				budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
				budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
				budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
				budgetPeriod.setBudgetPeriod(budgetPeriodNum);
				Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
				Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
				budgetPeriod.setStartDate(periodStartDateTimeStamp);
				budgetPeriod.setEndDate(periodEndDateTimeStamp);
				budgetPeriod.setBudget(budget);
				budgetPeriods.add(budgetPeriod);
				periodStartDate = nextPeriodStartDate;
				budgetPeriodNum++;
			}
		}
		return budgetPeriods;
	}

	@Override
	public String addBudgetPeriod(BudgetVO vo) {
		vo = updateProposalBudget(vo);
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<BudgetPeriod> updateBudgetPeriods = new ArrayList<>(budgetPeriods);
		Collections.copy(updateBudgetPeriods, budgetPeriods);
		BudgetPeriod lastPeriod = budgetDao.getMaxBudgetPeriodByBudgetId(budgetHeader.getBudgetId());
		BudgetPeriod newBudgetPeriod = new BudgetPeriod();
		newBudgetPeriod.setBudget(budgetHeader);
		newBudgetPeriod.setBudgetPeriod(lastPeriod.getBudgetPeriod() + 1);
		newBudgetPeriod.setTotalCost(BigDecimal.ZERO);
		newBudgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
		newBudgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
		newBudgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
		newBudgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
		newBudgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
		newBudgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
		newBudgetPeriod.setTotalFundRequested(BigDecimal.ZERO);
		newBudgetPeriod.setTotalModifiedDirectCost(BigDecimal.ZERO);
		newBudgetPeriod.setTotalFundRequestedIncludBenifit(BigDecimal.ZERO);
		newBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		newBudgetPeriod.setUpdateUser(vo.getUpdateUser());
		newBudgetPeriod = budgetDao.saveBudgetPeriod(newBudgetPeriod);
		updateBudgetPeriods.add(newBudgetPeriod);
		budgetHeader.getBudgetPeriods().clear();
		budgetHeader.getBudgetPeriods().addAll(updateBudgetPeriods);
		budgetDao.saveOrUpdateBudget(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public boolean budgetLineItemExists(BudgetHeader budget, Integer budgetPeriod) {
		boolean lineItemExists = false;
		List<BudgetDetail> budgetLineItems = budget.getBudgetPeriods().get(budgetPeriod).getBudgetDetails();
		/* check budget line item */
		for (BudgetDetail periodLineItem : budgetLineItems) {
			Integer lineItemPeriod = periodLineItem.getBudgetPeriod();
			if (budgetPeriod + 1 == lineItemPeriod) {
				lineItemExists = true;
				break;
			}
		}
		return lineItemExists;
	}

	/* call budget calculation service to calculate budget */
	@Override
	public void calculateBudget(BudgetHeader budget) {
		recalculateBudget(budget);
	}

	@Override
	public void recalculateBudget(BudgetHeader budget) {
		budgetCalculationService.calculateBudget(budget);
	}

	@Override
	public boolean isRateOverridden(BudgetPeriod budgetPeriod) {
		return false;
	}

	protected int getYear(Date date) {
		Calendar c1 = Calendar.getInstance();
		c1.setTime(new java.util.Date(date.getTime()));
		return c1.get(Calendar.YEAR);
	}

	@Override
	public List<CostElement> fetchSysGeneratedCostElements(String activityTypeCode) {
		List<CostElement> systemGeneratedCE = new ArrayList<>();
		if (activityTypeCode != null && Constants.ACTIVITY_TYPE_CODE_RESEARCH.equals(activityTypeCode)) {
			String overHeadResearchCostElementId = commonDao.getParameterValueAsString(Constants.BUDGET_RESEARCH_OH_ON);
			if (overHeadResearchCostElementId != null && !overHeadResearchCostElementId.isEmpty()) {
				CostElement overHeadResearchCostElement = budgetDao.fetchCostElementsById(overHeadResearchCostElementId);
				overHeadResearchCostElement.setSystemGeneratedCEType(Constants.BUDGET_RESEARCH_OH_ON);
				systemGeneratedCE.add(overHeadResearchCostElement);
			}
		} else {
			String overHeadCostElementId = commonDao.getParameterValueAsString(Constants.BUDGET_OH_ON);
			if (overHeadCostElementId != null && !overHeadCostElementId.isEmpty()) {
				CostElement overHeadCostElement = budgetDao.fetchCostElementsById(overHeadCostElementId);
				overHeadCostElement.setSystemGeneratedCEType(Constants.BUDGET_OH_ON);
				systemGeneratedCE.add(overHeadCostElement);
			}
		}
		String fringeCostElementId = commonDao.getParameterValueAsString(Constants.BUDGET_FRINGE_ON);
		if (fringeCostElementId != null && !fringeCostElementId.isEmpty()) {
			CostElement fringeCostElement = budgetDao.fetchCostElementsById(fringeCostElementId);
			fringeCostElement.setSystemGeneratedCEType(Constants.BUDGET_FRINGE_ON);
			systemGeneratedCE.add(fringeCostElement);
		}
		return systemGeneratedCE;
	}

	@Override
	public String getSyncBudgetRates(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		if (!budgetHeader.getProposalRates().isEmpty()) {
			proposalDao.deleteProposalBudgetRate(budgetHeader.getProposalRates());
		}
		budgetHeader.getProposalRates().clear();
		budgetDao.saveBudgetHeader(budgetHeader);
		String activityTypeCode = vo.getActivityTypeCode();
		Integer proposalId = vo.getProposalId();
		Set<String> rateClassTypes = new HashSet<>();
		Integer grantTypeCode = vo.getGrantTypeCode();
		List<FibiProposalRate> proposalRates = fetchFilteredProposalRates(budgetHeader, activityTypeCode, rateClassTypes, grantTypeCode);
		if (proposalRates != null && !proposalRates.isEmpty()) {
			for (FibiProposalRate proposalRate : proposalRates) {
				rateClassTypes.add(proposalRate.getRateClass().getDescription());
			}
		}
		vo.setRateClassTypes(rateClassTypes);
		budgetHeader.getProposalRates().addAll(proposalRates);
		budgetDao.saveBudgetHeader(budgetHeader);
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			calculate(budgetHeader, null, activityTypeCode);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		String budgetTabName = vo.getBudgetTabName();
		if (budgetTabName.equals(Constants.BUDGET_TAB_SIMPLE)) {
			vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		}
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String resetProposalRates(BudgetVO vo) {
		boolean isDefaultApplicableRateForInternal = commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEFAULT_APPLICABLE_RATE_FOR_INTERNAL_TYPE);
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		budgetDao.saveBudgetHeader(budgetHeader);
		String activityTypeCode = vo.getActivityTypeCode();
		Integer proposalId = vo.getProposalId();
		Integer grantTypeCode = vo.getGrantTypeCode();
		List<FibiProposalRate> proposalRates = budgetHeader.getProposalRates();
		if (proposalRates != null && !proposalRates.isEmpty()) {
			if (isDefaultApplicableRateForInternal) {
				for (FibiProposalRate proposalRate : proposalRates) {
					if (grantTypeCode != null && grantTypeCode.equals(Constants.GRANT_CALL_TYPE_INTERNAL) && proposalRate.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_OVERHEAD)) {
						proposalRate.setApplicableRate(BigDecimal.ZERO);
					} else {
						proposalRate.setApplicableRate(proposalRate.getInstituteRate());
					}
				}
			} else {
				for (FibiProposalRate proposalRate : proposalRates) {
					proposalRate.setApplicableRate(proposalRate.getInstituteRate());
				}
			}
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			calculate(budgetHeader, null, activityTypeCode);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		String budgetTabName = vo.getBudgetTabName();
		if (budgetTabName.equals(Constants.BUDGET_TAB_SIMPLE)) {
			vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		}
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<FibiProposalRate> fetchFilteredProposalRates(BudgetHeader budget, String activityTypeCode, Set<String> rateClassTypes, Integer grantTypeCode) {
		List<FibiProposalRate> proposalRates = new ArrayList<>();
		InstituteRate rateMTDC = budgetDao.fetchInstituteRateByDateLessthanMax(budget.getStartDate(), activityTypeCode, Constants.RATE_CLASS_CODE_MTDC, "1");
		if (rateMTDC != null) {
			proposalRates.add(prepareProposalRate(rateMTDC, budget, rateClassTypes, grantTypeCode));
		}
		fetchEmployeeBenifitsRates(proposalRates, budget.getStartDate(), activityTypeCode, budget, rateClassTypes, grantTypeCode);
		InstituteRate rateInflation = budgetDao.fetchInstituteRateByDateLessthanMax(budget.getStartDate(), activityTypeCode, Constants.RATE_CLASS_CODE_INFLATION, "1");
		if (rateInflation != null) {
			proposalRates.add(prepareProposalRate(rateInflation, budget, rateClassTypes, grantTypeCode));
		}
		List<InstituteRate> instituteRates = budgetDao.filterInstituteRateByDateRange(budget.getStartDate(), budget.getEndDate(), activityTypeCode, budget.getCampusFlag());
		if (instituteRates != null && !instituteRates.isEmpty()) {
			for (InstituteRate instituteRate : instituteRates) {
				proposalRates.add(prepareProposalRate(instituteRate, budget, rateClassTypes, grantTypeCode));
			}
		}
		if (rateClassTypes != null && !rateClassTypes.isEmpty()) {
			List<String> rateClassCodes = budgetDao.fetchRateClassCodesNotInParams(rateClassTypes);
			if (rateClassCodes != null && !rateClassCodes.isEmpty()) {
				for (String rateClassCode : rateClassCodes) {
					List<String> rateTypeCodes = budgetDao.fetchRateTypeCodesByRateClassCode(rateClassCode);
					if (rateTypeCodes != null && !rateTypeCodes.isEmpty()) {
						for (String rateTypeCode : rateTypeCodes) {
							InstituteRate otherRate = budgetDao.fetchInstituteRateByDateLessthanMax(budget.getStartDate(), activityTypeCode, rateClassCode, rateTypeCode);
							if (otherRate != null) {
								proposalRates.add(prepareProposalRate(otherRate, budget, rateClassTypes, grantTypeCode));
							}
						}
					}
				}
			}
		}
		return proposalRates;
	}

	public FibiProposalRate prepareProposalRate(InstituteRate instituteRate, BudgetHeader budget, Set<String> rateClassTypes, Integer grantTypeCode) {
		FibiProposalRate proposalRate = new FibiProposalRate();
		boolean isDefaultApplicableRateInternal = commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEFAULT_APPLICABLE_RATE_FOR_INTERNAL_TYPE);
		if (isDefaultApplicableRateInternal) {
			if (grantTypeCode != null && grantTypeCode.equals(Constants.GRANT_CALL_TYPE_INTERNAL) && instituteRate.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_OVERHEAD)) {
				proposalRate.setApplicableRate(BigDecimal.ZERO);
			} else {
				proposalRate.setApplicableRate(instituteRate.getInstituteRate());
			}
		} else {
			proposalRate.setApplicableRate(instituteRate.getInstituteRate());
		}
		proposalRate.setFiscalYear(instituteRate.getFiscalYear());
		proposalRate.setInstituteRate(instituteRate.getInstituteRate());
		String flag = instituteRate.getOnOffCampusFlag();
		if (flag.equalsIgnoreCase(Constants.NO)) {
			proposalRate.setOnOffCampusFlag(true);
		} else {
			proposalRate.setOnOffCampusFlag(false);
		}
		proposalRate.setBudgetHeader(budget);
		proposalRate.setRateClassCode(instituteRate.getRateClassCode());
		proposalRate.setRateTypeCode(instituteRate.getRateTypeCode());
		proposalRate.setStartDate(instituteRate.getStartDate());
		proposalRate.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		proposalRate.setUpdateUser(budget.getUpdateUser());
		proposalRate.setActivityTypeCode(instituteRate.getActivityTypeCode());
		proposalRate.setRateClass(instituteRate.getRateClass());
		proposalRate.setRateType(instituteRate.getRateType());
		proposalRate.setActivityType(instituteRate.getActivityType());
		rateClassTypes.add(instituteRate.getRateClass().getDescription());
		return proposalRate;
	}

	public void fetchEmployeeBenifitsRates(List<FibiProposalRate> proposalRates, Timestamp startDate, String activityTypeCode, BudgetHeader budget, Set<String> rateClassTypes, Integer grantTypeCode) {
		InstituteRate fullTimeRate = budgetDao.fetchInstituteRateByDateLessthanMax(startDate, activityTypeCode, Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS, "1");
		if (fullTimeRate != null) {
			proposalRates.add(prepareProposalRate(fullTimeRate, budget, rateClassTypes, grantTypeCode));
		}
		InstituteRate partTimeRate = budgetDao.fetchInstituteRateByDateLessthanMax(startDate, activityTypeCode, Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS, "2");
		if (partTimeRate != null) {
			proposalRates.add(prepareProposalRate(partTimeRate, budget, rateClassTypes, grantTypeCode));
		}
		InstituteRate stipendsRate = budgetDao.fetchInstituteRateByDateLessthanMax(startDate, activityTypeCode, Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS, "4");
		if (stipendsRate != null) {
			proposalRates.add(prepareProposalRate(stipendsRate, budget, rateClassTypes, grantTypeCode));
		}
		InstituteRate jHURate = budgetDao.fetchInstituteRateByDateLessthanMax(startDate, activityTypeCode, Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS, "5");
		if (jHURate != null) {
			proposalRates.add(prepareProposalRate(jHURate, budget, rateClassTypes, grantTypeCode));
		}
		InstituteRate wagesRate = budgetDao.fetchInstituteRateByDateLessthanMax(startDate, activityTypeCode, Constants.RATE_CLASS_CODE_EMPLOYEE_BENEFITS, "6");
		if (wagesRate != null) {
			proposalRates.add(prepareProposalRate(wagesRate, budget, rateClassTypes, grantTypeCode));
		}
	}

	@Override
	public String deleteBudgetPeriod(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<BudgetPeriod> updatedbudgetPeriods = new ArrayList<BudgetPeriod>(budgetPeriods);
		Collections.copy(updatedbudgetPeriods, budgetPeriods);
		int budgetPeriodNumber = 0;
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			if (budgetPeriod.getBudgetPeriodId() != null && budgetPeriod.getBudgetPeriodId().equals(vo.getBudgetPeriodId())) {
				budgetPeriodNumber = budgetPeriod.getBudgetPeriod();
				budgetPeriod = budgetDao.deleteBudgetPeriod(budgetPeriod);
				updatedbudgetPeriods.remove(budgetPeriod);
			}
		}
		budgetHeader.getBudgetPeriods().clear();
		budgetHeader.getBudgetPeriods().addAll(updatedbudgetPeriods);
		if (budgetPeriodNumber > 0) {
			updateBudgetPeriods(budgetPeriods, budgetPeriodNumber, true);
		}
		budgetHeader.setBudgetPeriods(budgetPeriods);
		updateBudgetHeader(budgetHeader);
		budgetHeader.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budgetHeader.setUpdateUserName(personDao.getPersonFullNameByPersonId(AuthenticatedUser.getLoginPersonId()));
		budgetDao.saveOrUpdateBudget(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			if (budget.getBudgetId().equals(budgetHeader.getBudgetId())) {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budgetHeader));
			} else {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
			}
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		vo.setStatus(true);
		vo.setMessage("Budget period deleted successfully");
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteBudgetLineItem(BudgetVO vo) {
		Integer deletedPeriodId = vo.getBudgetPeriodId();
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		String activityTypeCode = vo.getActivityTypeCode();
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				if (budgetPeriod.getBudgetPeriodId().equals(vo.getBudgetPeriodId())) {
					List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
					List<BudgetDetail> updatedBudgetDetails = new ArrayList<BudgetDetail>(budgetDetails);
					Collections.copy(updatedBudgetDetails, budgetDetails);
					for (BudgetDetail budgetDetail : budgetDetails) {
						if (budgetDetail.getBudgetDetailId() != null && budgetDetail.getBudgetDetailId().equals(vo.getBudgetDetailId())) {
							budgetDetail = budgetDao.deleteBudgetDetail(budgetDetail);
							updatedBudgetDetails.remove(budgetDetail);
						}
						if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT)) {
							if (activityTypeCode != null && updatedBudgetDetails.size() <= fetchSysGeneratedCESize(activityTypeCode)) {
								budgetDetail = budgetDao.deleteBudgetDetail(budgetDetail);
								updatedBudgetDetails.remove(budgetDetail);
								deletedPeriodId = budgetPeriod.getBudgetPeriodId();
							}
						}
					}
					budgetPeriod.getBudgetDetails().clear();
					budgetPeriod.getBudgetDetails().addAll(updatedBudgetDetails);
				}
			}
			if (budgetHeader.getIsAutoCalc() != null && !budgetHeader.getIsAutoCalc()) {
				budgetHeader = updateDeletedCost(budgetHeader, deletedPeriodId);
			} else {
				budgetHeader.setBudgetPeriods(budgetPeriods);
				updateBudgetHeader(budgetHeader);
			}
			budgetDao.saveBudgetHeader(budgetHeader);
			vo.setBudgetHeader(budgetHeader);
			List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(budgetHeader.getProposalId());
			vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
			vo.setStatus(true);
			vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
			if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
				updateProposalBudget(vo);
			}
			vo.setBudgetHeaderDetails(calculateBudgetHeaderDetails(vo.getBudgetHeader(), budgetHeaders));
			budgetDao.saveOrUpdateBudget(vo.getBudgetHeader());
			vo.setMessage("Budget detail deleted successfully");
			return commonDao.convertObjectToJSON(vo);
	}
	
	private List<BudgetHeaderDetail> calculateBudgetHeaderDetails(BudgetHeader budgetHeader, List<BudgetHeader> budgetHeaders) {
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			if (budget.getBudgetId().equals(budgetHeader.getBudgetId())) {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budgetHeader));
			} else {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
			}
		}
		return budgetHeaderDetails;
	}
	
	public BudgetHeader updateDeletedCost(BudgetHeader budgetHeader, Integer currentPeriodId) {
		BigDecimal totalFringeCost = BigDecimal.ZERO;
		BigDecimal totalFandACost = BigDecimal.ZERO;
		BigDecimal totalLineItemCost = BigDecimal.ZERO;
		BigDecimal totalFundRequested = BigDecimal.ZERO;
		BigDecimal totalCostSharingAmount = BigDecimal.ZERO;
		BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
		BigDecimal totalFringeCostCostShare = BigDecimal.ZERO;
		BigDecimal totalFringeCostFundRequested = BigDecimal.ZERO;
		BigDecimal totalFandACostShare = BigDecimal.ZERO;
		BigDecimal totalFandAFundRequested = BigDecimal.ZERO;
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			if (currentPeriodId.equals(budgetPeriod.getBudgetPeriodId())){
				List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
				if (budgetDetails != null && !budgetDetails.isEmpty()) {
					for (BudgetDetail budgetItemDetail : budgetDetails) {
						if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
							if (budgetItemDetail.getLineItemCost()!=null) {
							totalLineItemCost = totalLineItemCost.add(budgetItemDetail.getLineItemCost());
							}
							if (budgetItemDetail.getCostSharingAmount() != null) {
								totalCostSharingAmount = totalCostSharingAmount.add(budgetItemDetail.getCostSharingAmount());
							}
							if (budgetItemDetail.getSponsorRequestedAmount() != null) {
								totalFundRequested = totalFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
						}
						if (Boolean.TRUE.equals(budgetItemDetail.getIsSystemGeneratedCostElement())) {
							if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
								totalFringeCost = totalFringeCost.add(budgetItemDetail.getLineItemCost());
								totalFringeCostCostShare = totalFringeCostCostShare.add(budgetItemDetail.getCostSharingAmount());
								totalFringeCostFundRequested = totalFringeCostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
							if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(budgetItemDetail.getLineItemCost());
								totalFandACostShare = totalFandACostShare.add(budgetItemDetail.getCostSharingAmount());
								totalFandAFundRequested = totalFandAFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
							if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_RESEARCH_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(budgetItemDetail.getLineItemCost());
								totalFandACostShare = totalFandACost.add(budgetItemDetail.getCostSharingAmount());
								totalFandAFundRequested = totalFandACost.add(budgetItemDetail.getSponsorRequestedAmount());
							}
						}
					}
					budgetPeriod.setTotalDirectCost(totalFundRequested.add(totalFringeCostFundRequested).setScale(2, RoundingMode.HALF_UP));
					budgetPeriod.setTotalIndirectCost(totalFandAFundRequested.setScale(2, RoundingMode.HALF_UP));
					budgetPeriod.setTotalCost(budgetPeriod.getTotalDirectCost().add(budgetPeriod.getTotalIndirectCost()).setScale(2, RoundingMode.HALF_UP));
					budgetPeriod.setCostSharingAmount(totalCostSharingAmount.add(totalFandACostShare).add(totalFringeCostCostShare).setScale(2, RoundingMode.HALF_UP));
					budgetPeriod.setTotalInKind(budgetPeriod.getCostSharingAmount().add(budgetPeriod.getUnderrecoveryAmount().setScale(2, RoundingMode.HALF_UP)));
					budgetPeriod.setTotalModifiedDirectCost(budgetPeriod.getTotalModifiedDirectCost().setScale(2, RoundingMode.HALF_UP));
					budgetPeriod.setTotalOfTotalCost(budgetPeriod.getTotalCost().add(budgetPeriod.getTotalInKind()).setScale(2, RoundingMode.HALF_UP));
			} else {
				budgetPeriod.setTotalDirectCost(totalFundRequested);
				budgetPeriod.setTotalIndirectCost(totalFandAFundRequested);
				budgetPeriod.setTotalCost(budgetPeriod.getTotalDirectCost().add(budgetPeriod.getTotalIndirectCost()).setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setCostSharingAmount(totalCostSharingAmount);
				budgetPeriod.setTotalInKind(budgetPeriod.getCostSharingAmount().add(budgetPeriod.getUnderrecoveryAmount().setScale(2, RoundingMode.HALF_UP)));
				budgetPeriod.setTotalModifiedDirectCost(totalModifiedDirectCost);
				budgetPeriod.setTotalOfTotalCost(budgetPeriod.getTotalCost().add(budgetPeriod.getTotalInKind()).setScale(2, RoundingMode.HALF_UP));
		
			}
		 }
		}
		updateBudgetHeader(budgetHeader);
		return budgetHeader;
	}

	protected void updateBudgetPeriods(List<BudgetPeriod> budgetPeriods, int checkPeriod, boolean deletePeriod) {
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			Integer budPeriod = budgetPeriod.getBudgetPeriod();
			if (budPeriod >= checkPeriod) {
				int newPeriod = 0;
				if (deletePeriod) {
					newPeriod = budPeriod - 1;
				} else {
					newPeriod = budPeriod + 1;
				}
				budgetPeriod.setBudgetPeriod(newPeriod);
				List<BudgetDetail> budgetLineItems = budgetPeriod.getBudgetDetails();
				for (BudgetDetail periodLineItem : budgetLineItems) {
					periodLineItem.setBudgetPeriod(newPeriod);
				}
			}
		}
	}

	@Override
	public String copyBudgetPeriod(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		budgetDao.saveBudgetHeader(budgetHeader);
		BudgetPeriod copyPeriod = budgetDao.getPeriodById(vo.getCopyPeriodId());
		BudgetPeriod currentPeriod = budgetDao.getPeriodById(vo.getCurrentPeriodId());
		copyBudgetDetails(vo, copyPeriod, currentPeriod, vo.getUserName());	
		String activityTypeCode = vo.getActivityTypeCode();
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc()) && activityTypeCode != null) {
				calculate(budgetHeader, null, activityTypeCode);
		}
		calculateCost(budgetHeader);
		budgetDao.saveBudgetHeader(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			if (budget.getBudgetId().equals(budgetHeader.getBudgetId())) {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budgetHeader));
			} else {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
			}
		}
		vo.setBudgetHeader(budgetHeader);
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(vo.getGrantTypeCode()));
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		saveOrUpdateProposalBudget(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public BudgetHeader calculateCost(BudgetHeader budgetHeader) {
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			BigDecimal totalFringeCostforLineItem = BigDecimal.ZERO;
			BigDecimal totalFandACostforLineItem = BigDecimal.ZERO;
			BigDecimal totalFringeCostCS = BigDecimal.ZERO;
			BigDecimal totalFringeCostFundRequested = BigDecimal.ZERO;
			BigDecimal totalFandACostFundRequested = BigDecimal.ZERO;
			BigDecimal totalFandACostCS = BigDecimal.ZERO;
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			BigDecimal totalFundRequested = BigDecimal.ZERO;
			BigDecimal totalCostSharing = BigDecimal.ZERO;
			BigDecimal totalUnderRecovery = BigDecimal.ZERO;
			List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
			if (budgetDetails != null && !budgetDetails.isEmpty()) {
				for (BudgetDetail budgetItemDetail : budgetDetails) {
					if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
						totalLineItemCost = totalLineItemCost.add(budgetItemDetail.getLineItemCost());
						if (budgetItemDetail.getSponsorRequestedAmount() != null) {
							totalFundRequested = totalFundRequested .add(budgetItemDetail.getSponsorRequestedAmount());
						}
						if (budgetItemDetail.getCostSharingAmount() != null) {
							totalCostSharing = totalCostSharing.add(budgetItemDetail.getCostSharingAmount());
						}
						//when auto calculate is off underrecovery is not needed
					} else {
							if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							if (budgetItemDetail.getSponsorRequestedAmount() != null) {
								totalFringeCostFundRequested = totalFringeCostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
							if (budgetItemDetail.getCostSharingAmount() != null) {
								totalFringeCostCS = totalFringeCostCS.add(budgetItemDetail.getCostSharingAmount());
							}
							if (budgetItemDetail.getLineItemCost() != null) {
								totalFringeCostforLineItem = totalFringeCostforLineItem.add(budgetItemDetail.getLineItemCost());
							}
							}
						if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							if (budgetItemDetail.getSponsorRequestedAmount() != null) {
								totalFandACostFundRequested = totalFandACostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
							if (budgetItemDetail.getCostSharingAmount() != null) {
								totalFandACostCS = totalFandACostCS.add(budgetItemDetail.getCostSharingAmount());
							}
							if (budgetItemDetail.getLineItemCost() != null) {
								totalFandACostforLineItem = totalFandACostforLineItem.add(budgetItemDetail.getLineItemCost());
							}
						}
						if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_RESEARCH_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							if (budgetItemDetail.getSponsorRequestedAmount() != null) {
								totalFandACostFundRequested = totalFandACostFundRequested.add(budgetItemDetail.getSponsorRequestedAmount());
							}
							if (budgetItemDetail.getCostSharingAmount() != null) {
								totalFandACostCS = totalFandACostCS.add(budgetItemDetail.getCostSharingAmount());
							}
							if (budgetItemDetail.getLineItemCost() != null) {
								totalFandACostforLineItem = totalFandACostforLineItem.add(budgetItemDetail.getLineItemCost());
							}
						}
					 }
					}
				budgetPeriod.setTotalDirectCost(totalFundRequested.add(totalFringeCostFundRequested).setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalIndirectCost(totalFandACostFundRequested.setScale(2, RoundingMode.HALF_UP));
				totalCostSharing = totalCostSharing.add(totalFandACostCS).add(totalFringeCostCS);
				budgetPeriod.setCostSharingAmount(totalCostSharing.setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setUnderrecoveryAmount(totalUnderRecovery);
				budgetPeriod.setTotalInKind(budgetPeriod.getTotalInKind().setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalOfTotalCost(budgetPeriod.getTotalOfTotalCost().setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalModifiedDirectCost(budgetPeriod.getTotalModifiedDirectCost().setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalCost(budgetPeriod.getTotalDirectCost().add(budgetPeriod.getTotalIndirectCost()).setScale(2,RoundingMode.HALF_UP));
			}
		}
		updateBudgetHeader(budgetHeader);
		return budgetHeader;
	}

	public BudgetDetail deleteBudgetDetailCalcAmount(BudgetDetail budgetDetail) {
		if (budgetDetail.getBudgetDetailCalcAmounts() != null && !budgetDetail.getBudgetDetailCalcAmounts().isEmpty()) {
			List<BudgetDetailCalcAmount> budgetDetailCalcAmounts = budgetDetail.getBudgetDetailCalcAmounts();
			List<BudgetDetailCalcAmount> updatedbudgetDetailCalcAmounts = new ArrayList<BudgetDetailCalcAmount>(budgetDetailCalcAmounts);
			Collections.copy(updatedbudgetDetailCalcAmounts, budgetDetailCalcAmounts);
			for (BudgetDetailCalcAmount budgetDetailCalcAmount : budgetDetailCalcAmounts) {
				budgetDetailCalcAmount = budgetDao.deleteBudgetDetailCalcAmount(budgetDetailCalcAmount);
				updatedbudgetDetailCalcAmounts.remove(budgetDetailCalcAmount);
			}
			budgetDetail.getBudgetDetailCalcAmounts().clear();
			budgetDetail.getBudgetDetailCalcAmounts().addAll(updatedbudgetDetailCalcAmounts);
			budgetDetail = budgetDao.saveBudgetDetail(budgetDetail);
		}
		return budgetDetail;
	}

	@Override
	public String generateBudgetPeriods(BudgetVO vo) {
		BudgetHeader budgetHeader = new BudgetHeader();
		budgetHeader = vo.getBudgetHeader();
		budgetDao.saveBudgetHeader(budgetHeader);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		BudgetPeriod copyPeriod = budgetDao.getPeriodById(vo.getCurrentPeriodId());
		for (BudgetPeriod currentPeriod : budgetPeriods) {
			if (!currentPeriod.getBudgetPeriodId().equals(vo.getCurrentPeriodId())) {
				copyBudgetDetails(vo, copyPeriod, currentPeriod, vo.getUserName());
			}
			copyPeriod = currentPeriod;
		}
		if (budgetHeader.getIsAutoCalc() != null && !budgetHeader.getIsAutoCalc()) {
			budgetHeader = calculateCost(budgetHeader);
		} else {
			updateProposalBudget(vo);
		}
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		saveOrUpdateProposalBudget(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void copyBudgetDetails(BudgetVO budgetVO, BudgetPeriod copyPeriod, BudgetPeriod currentPeriod, String userName) {
		BudgetHeader budgetHeader = budgetVO.getBudgetHeader();
		List<BudgetDetail> budgetDetails = copyPeriod.getBudgetDetails();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			List<BudgetDetail> copiedBudgetDetails = new ArrayList<>(budgetDetails);
			Collections.copy(copiedBudgetDetails, budgetDetails);
			List<BudgetDetail> newLineItems = new ArrayList<>();
			for (BudgetDetail budgetDetail : copiedBudgetDetails) {
				BudgetDetail detail = new BudgetDetail();
				detail.setBudgetCategory(budgetDetail.getBudgetCategory());
				detail.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
				detail.setBudgetJustification(budgetDetail.getBudgetJustification());
				detail.setBudgetPeriod(currentPeriod.getBudgetPeriod());
				detail.setEndDate(budgetDetail.getEndDate());
				detail.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
				detail.setSystemGeneratedCEType(budgetDetail.getSystemGeneratedCEType());
				detail.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
				detail.setCostSharingAmount(budgetDetail.getCostSharingAmount());
				detail.setCostSharingPercentage(budgetDetail.getCostSharingPercentage());
				detail.setQuantity(budgetDetail.getQuantity());
				detail.setLineItemNumber(budgetDetail.getLineItemNumber());
				detail.setLineItemDescription(budgetDetail.getLineItemDescription());
				detail.setSponsorRequestedAmount(budgetDetail.getSponsorRequestedAmount());
				detail.setTotalModifiedDirectCost(budgetDetail.getTotalModifiedDirectCost());
				detail.setCalculateCostSharingWithBenifit(budgetDetail.getCalculateCostSharingWithBenifit());
				detail.setCalculateLineItemCostIncludingBenifit(budgetDetail.getCalculateLineItemCostIncludingBenifit());
				detail.setCalculateFundRequestedIncludingBenifit(budgetDetail.getCalculateFundRequestedIncludingBenifit());
				detail.setUnderrecoveryAmount(budgetDetail.getUnderrecoveryAmount());
				// apply inflation here
				CostElement costElement = budgetDetail.getCostElement();
				costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
				detail.setCostElement(costElement);
				detail.setCostElementCode(budgetDetail.getCostElementCode());
				BigDecimal lineItemCost = budgetDetail.getLineItemCost();
				BigDecimal updatedLineItemCost = BigDecimal.ZERO;
				List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
				BudgetDetailCalcAmount budgetCalculatedAmount = null;
				if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
					for (ValidCeRateType ceRateType : ceRateTypes) {
						FibiProposalRate applicableRate = budgetDao.fetchApplicableProposalRate(copyPeriod.getBudget().getBudgetId(), copyPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), budgetVO.getActivityTypeCode());
						if (applicableRate != null && (applicableRate.getRateClass().getRateClassTypeCode().equals(Constants.RATE_CLASS_CODE_TYPE_INFLATION) && Constants.RATE_CLASS_CODE_INFLATION.equals(applicableRate.getRateClassCode()))) {
							BigDecimal validRate = BigDecimal.ZERO;
							validRate = validRate.add(applicableRate.getApplicableRate());
							if (validRate.compareTo(BigDecimal.ZERO) > 0) {
								BigDecimal hundred = new BigDecimal(100);
								BigDecimal percentageFactor = validRate.divide(hundred);
								BigDecimal calculatedCost = ((lineItemCost.multiply(percentageFactor)));
								updatedLineItemCost = updatedLineItemCost.add(calculatedCost);
								budgetCalculatedAmount = getNewBudgetCalculatedAmount(currentPeriod, budgetDetail, applicableRate);
								budgetCalculatedAmount.setCalculatedCost(calculatedCost);
								if (budgetDetail.getIsApplyInflationRate().equals(true) && budgetHeader.getIsAutoCalc() != null && budgetHeader.getIsAutoCalc()) {
									detail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
								}
							}
						}
					}
				}
				if (updatedLineItemCost.compareTo(BigDecimal.ZERO) > 0) {
					if (budgetDetail.getIsApplyInflationRate().equals(true) && budgetHeader.getIsAutoCalc() != null && Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
						lineItemCost = lineItemCost.add(updatedLineItemCost);
						if (lineItemCost != null) {
							detail.setLineItemCost(lineItemCost.setScale(2, RoundingMode.HALF_UP));
						}
					} else {
						if (lineItemCost != null) {
							detail.setLineItemCost(lineItemCost.setScale(2, RoundingMode.HALF_UP));
						}
					}
				} else {
					if (lineItemCost != null) {
						detail.setLineItemCost(lineItemCost.setScale(2, RoundingMode.HALF_UP));
					}
				}
				detail.setLineItemDescription(budgetDetail.getLineItemDescription());
				detail.setLineItemNumber(budgetDetail.getLineItemNumber());
				detail.setOnOffCampusFlag(budgetDetail.getOnOffCampusFlag());
				currentPeriod.setUnderrecoveryAmount(copyPeriod.getUnderrecoveryAmount());
				detail.setPeriod(currentPeriod);
				detail.setPrevLineItemCost(budgetDetail.getPrevLineItemCost());
				detail.setStartDate(budgetDetail.getStartDate());
				detail.setUpdateTimeStamp(budgetDetail.getUpdateTimeStamp());
				detail.setUpdateUser(userName);
				detail.setPersonsDetails(copyBudgetPersonDetails(budgetDetail.getPersonsDetails(), detail, currentPeriod));
				detail = budgetDao.saveBudgetDetail(detail);
				newLineItems.add(detail);
			}
			currentPeriod.getBudgetDetails().clear(); // for single page budget
			currentPeriod.getBudgetDetails().addAll(newLineItems);
		} else {
			currentPeriod.setTotalDirectCost(copyPeriod.getTotalDirectCost());
			currentPeriod.setTotalIndirectCost(copyPeriod.getTotalIndirectCost());
			currentPeriod.setTotalCost(copyPeriod.getTotalCost());
			currentPeriod.setCostSharingAmount(copyPeriod.getCostSharingAmount());
			currentPeriod.setUnderrecoveryAmount(copyPeriod.getUnderrecoveryAmount());
			currentPeriod.setUpdateUser(copyPeriod.getUpdateUser());
			currentPeriod.setTotalInKind(copyPeriod.getTotalInKind());
			currentPeriod.setTotalModifiedDirectCost(copyPeriod.getTotalModifiedDirectCost());
			currentPeriod.setTotalOfTotalCost(copyPeriod.getTotalOfTotalCost());
			currentPeriod.setUpdateTimeStamp(copyPeriod.getUpdateTimeStamp());
		}
	}

	@Override
	public String copyProposalBudget(BudgetVO vo) {
		Integer proposalId = vo.getProposalId();
		Integer newProposalId = vo.getCopiedProposalId();
		String copyType = vo.getCopyType();
		BudgetHeader orginalBudget = null;
		List<BudgetHeader> budgets = budgetDao.fetchBudgetsByProposalId(proposalId);
		if (budgets != null && !budgets.isEmpty() && copyType == null) {
			for (BudgetHeader budgetHeader : budgets) {
				if (budgetHeader.getIsLatestVersion()) {
					budgetHeader.setIsLatestVersion(false);
					budgetDao.saveBudgetHeader(budgetHeader);
				}
			}
		}
		if (vo.getBudgetId() != null) {
			orginalBudget = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
		}
		BudgetHeader copyBudget = new BudgetHeader();
		Integer budgetVersionNumber = null;
		if (copyType == null) {
			 budgetVersionNumber = budgetDao.maxBudgetVersionNumberByProposalId(proposalId);
		} else if (copyType.equals(Constants.ARCHIVE_TYPE_PROPOSAL)) {
			 budgetVersionNumber = orginalBudget.getVersionNumber() - 1;
		} else {
			budgetVersionNumber = budgetDao.maxBudgetVersionNumberByProposalId(newProposalId);
		}
		if (budgetVersionNumber == null) {
			budgetVersionNumber = 1;
		} else {
			budgetVersionNumber = budgetVersionNumber + 1;
		}
		copyBudget.setOnCampusRates(orginalBudget.getOnCampusRates());
		copyBudget.setOffCampusRates(orginalBudget.getOffCampusRates());
		copyBudget.setCostSharingTypeCode(orginalBudget.getCostSharingTypeCode());
		if (orginalBudget != null) {
			if (newProposalId != null) {
				copyBudget.setProposalId(newProposalId);
			} else {
				copyBudget.setProposalId(orginalBudget.getProposalId());
			}
			createBudgetHeader(copyBudget, orginalBudget, vo.getUserName(), vo.getUserFullName(), budgetVersionNumber, copyType);
			Set<String> rateClassTypes = new HashSet<>();
			for (FibiProposalRate proposalRate : orginalBudget.getProposalRates()) {
				FibiProposalRate fibiProposalRate = new FibiProposalRate();
				fibiProposalRate.setProposalRateId(null);
				fibiProposalRate.setActivityType(proposalRate.getActivityType());
				fibiProposalRate.setActivityTypeCode(proposalRate.getActivityTypeCode());
				fibiProposalRate.setApplicableRate(proposalRate.getApplicableRate());
				fibiProposalRate.setBudgetHeader(copyBudget);
				fibiProposalRate.setFiscalYear(proposalRate.getFiscalYear());
				fibiProposalRate.setInstituteRate(proposalRate.getInstituteRate());
				fibiProposalRate.setOnOffCampusFlag(proposalRate.getOnOffCampusFlag());
				fibiProposalRate.setRateClass(proposalRate.getRateClass());
				fibiProposalRate.setRateClassCode(proposalRate.getRateClassCode());
				fibiProposalRate.setRateType(proposalRate.getRateType());
				fibiProposalRate.setRateTypeCode(proposalRate.getRateTypeCode());
				fibiProposalRate.setStartDate(proposalRate.getStartDate());
				fibiProposalRate.setUpdateTimeStamp(proposalRate.getUpdateTimeStamp());
				fibiProposalRate.setUpdateUser(proposalRate.getUpdateUser());
				copyBudget.getProposalRates().add(fibiProposalRate);
			}
			vo.setRateClassTypes(rateClassTypes);
			copyBudget = budgetDao.saveBudgetHeader(copyBudget);
			if (orginalBudget.getBudgetPeriods() != null && !orginalBudget.getBudgetPeriods().isEmpty()) {
				copyBudget.getBudgetPeriods().addAll(proposalCopyService.copyBudgetPeriods(copyBudget, orginalBudget, vo.getActivityTypeCode(), vo.getUserName()));
			}
		}
		if (copyBudget.getIsAutoCalc() != null && !copyBudget.getIsAutoCalc()) {
			copyBudget = calculateCost(copyBudget);
		} else {
			updateBudgetHeader(copyBudget);
		}
		budgetDao.saveBudgetHeader(copyBudget);
		vo.setBudgetHeader(copyBudget);
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(copyBudget.getProposalId()));
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(copyBudget.getBudgetId()));
		if (Boolean.TRUE.equals(copyBudget.getIsAutoCalc())) {
			calculateBudgetDetails(copyBudget, vo.getActivityTypeCode());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void createBudgetHeader(BudgetHeader copyBudget, BudgetHeader originalBudget, String userName, String userFullName, Integer budgetVersionNumber, String copyType) {
		copyBudget.setStartDate(originalBudget.getStartDate());
		copyBudget.setEndDate(originalBudget.getEndDate());
		copyBudget.setCreateTimeStamp(commonDao.getCurrentTimestamp());
		copyBudget.setCreateUser(userName);
		copyBudget.setCreateUserName(userFullName);
		copyBudget.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyBudget.setUpdateUser(userName);
		copyBudget.setUpdateUserName(userFullName);
		copyBudget.setRateType(originalBudget.getRateType());
		copyBudget.setComments(originalBudget.getComments());
		copyBudget.setRateClassCode(originalBudget.getRateClassCode());
		copyBudget.setRateTypeCode(originalBudget.getRateTypeCode());
		copyBudget.setIsAutoCalc(originalBudget.getIsAutoCalc());
		copyBudget.setBudgetTemplateTypeId(originalBudget.getBudgetTemplateTypeId());
		copyBudget.setVersionNumber(budgetVersionNumber);
		if (originalBudget.getBudgetStatusCode().equals(Constants.BUDGET_STATUS_SUBMITTED)) {
			copyBudget.setBudgetStatusCode(Constants.BUDGET_STATUS_INPROGRESS);
			copyBudget.setBudgetStatus(budgetDao.getBudgetStatusById(Constants.BUDGET_STATUS_INPROGRESS));
		} else {
			copyBudget.setBudgetStatusCode(originalBudget.getBudgetStatusCode());
			copyBudget.setBudgetStatus(originalBudget.getBudgetStatus());
		}
		copyBudget.setUnderrecoveryRateClassCode(originalBudget.getUnderrecoveryRateClassCode());
		copyBudget.setUnderrecoveryRateTypeCode(originalBudget.getUnderrecoveryRateTypeCode());
		copyBudget.setUnderrecoveryRateType(originalBudget.getUnderrecoveryRateType());
		copyBudget.setCampusFlag(originalBudget.getCampusFlag());
		if (copyType != null && (copyType.equals(Constants.COPY_TYPE_PROPOSAL) || copyType.equals(Constants.ARCHIVE_TYPE_PROPOSAL))) {
			copyBudget.setIsLatestVersion(originalBudget.getIsLatestVersion());
			copyBudget.setIsFinalBudget(originalBudget.getIsFinalBudget());
			if (copyType.equals(Constants.ARCHIVE_TYPE_PROPOSAL)) {
				copyBudget.setIsApprovedBudget(originalBudget.getIsApprovedBudget());
			}
		} else if (copyType != null && copyType.equals(Constants.COPY_APPROVED_BUDGET)) {
			copyBudget.setIsApprovedBudget(true);
		} else {
			copyBudget.setIsLatestVersion(true);
			copyBudget.setIsSelected(true);
		}
	}

	@Override
	public String deleteSimpleBudgetLineItem(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		Boolean isSysGenCostElementRequired = commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
			List<BudgetDetail> updatedbudgetDetails = new ArrayList<BudgetDetail>(budgetDetails);
			Collections.copy(updatedbudgetDetails, budgetDetails);
			for (BudgetDetail budgetDetail : budgetDetails) {
				if (Boolean.TRUE.equals(budgetDetail.getIsDeleted())) {
					budgetDetail = budgetDao.deleteBudgetDetail(budgetDetail);
					updatedbudgetDetails.remove(budgetDetail);
				}
				if (Boolean.TRUE.equals(isSysGenCostElementRequired) && updatedbudgetDetails.size() <= fetchSysGeneratedCESize(vo.getActivityTypeCode())) {
					budgetDetail = budgetDao.deleteBudgetDetail(budgetDetail);
					updatedbudgetDetails.remove(budgetDetail);
				}
			}
			budgetPeriod.getBudgetDetails().clear();
			budgetPeriod.getBudgetDetails().addAll(updatedbudgetDetails);
		}
		vo.setBudgetHeader(budgetHeader);
		saveOrUpdateProposalBudget(vo);
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		if (Boolean.TRUE.equals(isSysGenCostElementRequired)) {
			vo.setSysGeneratedCostElements(fetchSysGeneratedCostElements(vo.getActivityTypeCode()));
		}
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		vo.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(vo.getGrantTypeCode()));
		vo.setStatus(true);
		vo.setMessage("Budget detail deleted successfully");
		return commonDao.convertObjectToJSON(vo);
	}

	public BudgetHeader updateSimpleDeletedCost(BudgetHeader budgetHeader) {
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			BigDecimal totalFringeCost = BigDecimal.ZERO;
			BigDecimal totalFandACost = BigDecimal.ZERO;
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			BigDecimal totalCostSharingAmount = BigDecimal.ZERO;
			BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
			List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
			if (budgetDetails != null && !budgetDetails.isEmpty()) {
				for (BudgetDetail budgetItemDetail : budgetDetails) {
					if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
						totalLineItemCost = totalLineItemCost.add(budgetItemDetail.getLineItemCost());
						if (budgetItemDetail.getCostSharingAmount() != null) {
							totalCostSharingAmount = totalCostSharingAmount.add(budgetItemDetail.getCostSharingAmount());
						}
					}
				}
				for (BudgetDetail budgetItemDetail : budgetDetails) {
					if (Boolean.TRUE.equals(budgetItemDetail.getIsSystemGeneratedCostElement())) {
						if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							totalFringeCost = totalFringeCost.add(budgetItemDetail.getLineItemCost());
						}
						if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							totalFandACost = totalFandACost.add(budgetItemDetail.getLineItemCost());
						}
						if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType()) || Constants.BUDGET_RESEARCH_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
							totalFandACost = totalFandACost.add(budgetItemDetail.getLineItemCost());
						}
					}
				}
				budgetPeriod.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalCost(totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setCostSharingAmount(totalCostSharingAmount.setScale(2, RoundingMode.HALF_UP));
				budgetPeriod.setTotalModifiedDirectCost(budgetPeriod.getTotalModifiedDirectCost().setScale(2, RoundingMode.HALF_UP));
			} else {
				budgetPeriod.setTotalDirectCost(totalLineItemCost);
				budgetPeriod.setTotalIndirectCost(totalFandACost);
				budgetPeriod.setTotalCost(totalLineItemCost);
				budgetPeriod.setCostSharingAmount(totalCostSharingAmount);
				budgetPeriod.setTotalModifiedDirectCost(totalModifiedDirectCost);
			}
		}
		updateBudgetHeader(budgetHeader);
		return budgetHeader;
	}

	@Override
	public String addSimpleBudgetPeriod(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		budgetDao.saveOrUpdateBudget(budgetHeader);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<BudgetPeriod> updateBudgetPeriods = new ArrayList<>(budgetPeriods);
		Collections.copy(updateBudgetPeriods, budgetPeriods);
		BudgetPeriod lastPeriod = budgetDao.getMaxBudgetPeriodByBudgetId(budgetHeader.getBudgetId());
		BudgetPeriod newBudgetPeriod = new BudgetPeriod();
		newBudgetPeriod.setBudget(budgetHeader);
		newBudgetPeriod.setBudgetPeriod(lastPeriod.getBudgetPeriod() + 1);
		newBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		newBudgetPeriod.setUpdateUser(vo.getUpdateUser());
		newBudgetPeriod = budgetDao.saveBudgetPeriod(newBudgetPeriod);
		copySimpleBudgetDetails(lastPeriod, newBudgetPeriod, vo.getUserName());
		updateBudgetPeriods.add(newBudgetPeriod);
		budgetHeader.getBudgetPeriods().clear();
		budgetHeader.getBudgetPeriods().addAll(updateBudgetPeriods);
		if (budgetHeader.getIsAutoCalc() != null && !budgetHeader.getIsAutoCalc()) {
			budgetHeader = calculateCost(budgetHeader);
		}
		budgetDao.saveOrUpdateBudget(budgetHeader);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		loadBudgetInitialData(vo);
		vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	private void copySimpleBudgetDetails(BudgetPeriod copyPeriod, BudgetPeriod currentPeriod, String userName) {
		List<BudgetDetail> budgetDetails = copyPeriod.getBudgetDetails();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			List<BudgetDetail> copiedBudgetDetails = new ArrayList<>(budgetDetails);
			Collections.copy(copiedBudgetDetails, budgetDetails);
			List<BudgetDetail> newLineItems = new ArrayList<>();
			for (BudgetDetail budgetDetail : copiedBudgetDetails) {
				BudgetDetail detail = new BudgetDetail();
				detail.setBudgetCategory(budgetDetail.getBudgetCategory());
				detail.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
				detail.setBudgetJustification(budgetDetail.getBudgetJustification());
				detail.setBudgetPeriod(currentPeriod.getBudgetPeriod());
				detail.setEndDate(budgetDetail.getEndDate());
				detail.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
				detail.setSystemGeneratedCEType(budgetDetail.getSystemGeneratedCEType());
				detail.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
				CostElement costElement = budgetDetail.getCostElement();
				costElement = budgetDao.fetchCostElementsById(costElement.getCostElement());
				detail.setCostElement(costElement);
				detail.setCostElementCode(budgetDetail.getCostElementCode());
				detail.setLineItemDescription(budgetDetail.getLineItemDescription());
				detail.setLineItemNumber(budgetDetail.getLineItemNumber());
				detail.setOnOffCampusFlag(budgetDetail.getOnOffCampusFlag());
				detail.setPeriod(currentPeriod);
				if (Boolean.TRUE.equals(currentPeriod.getGeneratePeriod())) {
					detail.setLineItemCost(budgetDetail.getLineItemCost());
				}
				detail.setPrevLineItemCost(budgetDetail.getPrevLineItemCost());
				detail.setStartDate(budgetDetail.getStartDate());
				detail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				detail.setUpdateUser(userName);
				detail.setFullName(budgetDetail.getFullName());
				detail.setRolodexId(budgetDetail.getRolodexId());
				detail.setPersonId(budgetDetail.getPersonId());
				detail.setTbnId(budgetDetail.getTbnId());
				detail.setTbnPerson(budgetDetail.getTbnPerson());
				detail.setPersonType(budgetDetail.getPersonType());
				newLineItems.add(detail);
			}
			currentPeriod.getBudgetDetails().clear(); // for single page budget
			currentPeriod.getBudgetDetails().addAll(newLineItems);
		}
	}

	@Override
	public String deletePersonnelLine(BudgetVO vo) {
		BudgetPersonalDetails personDetails = budgetDao.getBudgetPersonalDetailsById(vo.getBudgetPersonDetailId());
		updateBudgetDetailAfterDeleteLineItem(personDetails, vo);
		budgetDao.deleteBudgetPersonDetail(vo.getBudgetPersonDetailId());
		vo.setStatus(true);
		vo.setMessage("Budget person deleted successfully");
		return commonDao.convertObjectToJSON(vo);
	}
	
	private void updateBudgetDetailAfterDeleteLineItem(BudgetPersonalDetails personDetails, BudgetVO vo) {
		BudgetDetail budgetDetail = budgetDao.fetchBudgetDetailBasedOnBudgetDetailId(personDetails.getBudgetDetail().getBudgetDetailId());
		BigDecimal lineItemCost = BigDecimal.ZERO;
		BigDecimal fundRequestAmount = BigDecimal.ZERO;
		BigDecimal costShareAmount = BigDecimal.ZERO;
		lineItemCost = budgetDetail.getLineItemCost().subtract(personDetails.getSalaryRequested());
		fundRequestAmount = budgetDetail.getSponsorRequestedAmount().subtract(personDetails.getSponsorRequestedAmount());
		costShareAmount = budgetDetail.getCostSharingAmount().subtract(personDetails.getCostSharingAmount());
		budgetDetail.setLineItemCost(lineItemCost);
		budgetDetail.setCostSharingAmount(costShareAmount);
		budgetDetail.setSponsorRequestedAmount(fundRequestAmount);
		budgetDao.saveBudgetDetail(budgetDetail);
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_PERSONNEL_LINE_ITEM_DELETE_ON_LAST_PERSON_DELETE)) {
				if (lineItemCost.compareTo(BigDecimal.ZERO) == 0 || lineItemCost.compareTo(BigDecimal.ZERO) == -1) {
				budgetDao.deleteBudgetDetail(budgetDetail);
				budgetDetail.setLineItemCost(BigDecimal.ZERO);
				budgetDetail.setCostSharingAmount(BigDecimal.ZERO);
				budgetDetail.setSponsorRequestedAmount(BigDecimal.ZERO);
				}
			}
		vo.setBudgetPeriodId(budgetDetail.getPeriod().getBudgetPeriodId());
		vo.setBudgetPeriod(budgetDetail.getBudgetPeriod());
	}

	@Override
	public List<BudgetPeriod> generateBudgetPeriodsByCalendarYear(BudgetHeader budget) {
		List<BudgetPeriod> budgetPeriods = new ArrayList<>();
		Date projectStartDate = new Date(budget.getStartDate().getTime());
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;
		int budgetPeriodEndingYear = getYear(projectEndDate);
		Calendar cl = Calendar.getInstance();
		Date periodStartDate = projectStartDate;
		int budgetPeriodNum = 1;
		if (commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_BUDGET_ENABLE_SINGLE_PERIOD)) {
				cl.setTime(periodStartDate);
				BudgetPeriod budgetPeriod = new BudgetPeriod();
				budgetPeriod.setBudgetPeriod(budgetPeriodNum);
				budgetPeriod.setStartDate(budget.getStartDate());
				budgetPeriod.setEndDate(budget.getEndDate());
				budgetPeriod.setBudget(budget);
				budgetPeriod.setTotalCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
				budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
				budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
				budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
				budgetPeriods.add(budgetPeriod);
		} else {
			while (budgetPeriodExists) {
				cl.setTime(periodStartDate);
				cl.add(Calendar.DATE, 1);
				Date periodEndDate = new Date(cl.getTime().getTime());
				int currentPeriodYear = cl.get(Calendar.YEAR);
				if (currentPeriodYear < budgetPeriodEndingYear) {
					periodEndDate = getEndDateForaYear(currentPeriodYear);
				} else {
					periodEndDate = projectEndDate;
				}
				cl.setTime(periodEndDate);
				Date nextPeriodStartDate = new Date(cl.getTime().getTime());
				switch (periodEndDate.compareTo(projectEndDate)) {
				case 1:
					periodEndDate = projectEndDate;
					// the break statement is purposefully missing.
				case 0:
					budgetPeriodExists = false;
					break;
				}
				BudgetPeriod budgetPeriod = new BudgetPeriod();
				budgetPeriod.setBudgetPeriod(budgetPeriodNum);
				Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
				Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
				budgetPeriod.setStartDate(periodStartDateTimeStamp);
				budgetPeriod.setEndDate(periodEndDateTimeStamp);
				budgetPeriod.setBudget(budget);
				budgetPeriod.setTotalCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
				budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
				budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
				budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
				budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
				budgetPeriods.add(budgetPeriod);
				cl.setTime(new Timestamp(nextPeriodStartDate.getTime()));
				cl.add(Calendar.DATE, 1);
				periodStartDate = new Date(cl.getTime().getTime());
				budgetPeriodNum++;
			}
		}
		return budgetPeriods;
	}

	private Date getEndDateForaYear(int year) {
		Date date = new Date();
		SimpleDateFormat formatter = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
		String dateInString = "31/12/".concat(String.valueOf(year));
		try {
			date = formatter.parse(dateInString);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return date;
	}

	private Date getStartDateForaYear(int year) {
		Date date = new Date();
		SimpleDateFormat formatter = new SimpleDateFormat(Constants.DEFAULT_DATE_FORMAT);
		String dateInString = "01/01/".concat(String.valueOf(year));
		try {
			date = formatter.parse(dateInString);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return date;
	}


	private boolean isPeriodGenerationBasedOnCalenderYear() {
		return commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_BUDGET_PERIOD_GENERATION_BASED_ON_CALENDAR_YEAR);
	}

	@Override
	public String fetchBudgetSummaryTable(Integer budgetHeaderId) {
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetHeaderId);
		return commonDao.convertObjectToJSON(prepareBudgetSummary(budgetHeader));
	}

	private BudgetSummary prepareBudgetSummary(BudgetHeader budgetHeader) {
		BigDecimal totalUnderRecoveryAmount = BigDecimal.ZERO;
		Set<String> budgetCategoryCodes = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		List<BudgetSummaryVO> budgetSummaryVOs = new ArrayList<>();
		BudgetSummary budgetSummary = new BudgetSummary();
		List<BudgetPeriodSummary> budgetPeriodSummaries = new ArrayList<>();
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<BudgetDetail> budgetDetails = new ArrayList<>();
		List<Integer> budgetPeriodIds = new ArrayList<>();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			budgetPeriodIds.add(budgetPeriod.getBudgetPeriodId());
			budgetDetails.addAll(budgetPeriod.getBudgetDetails());
			BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
			budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
			budgetSummaryVO.setLineItemCost(budgetPeriod.getTotalOfTotalCost());
			budgetSummaryVO.setTotalFundRequested(budgetPeriod.getTotalCost());
			budgetSummaryVO.setTotalCostSharingAmount(budgetPeriod.getCostSharingAmount());
			budgetSummaryVOs.add(budgetSummaryVO);
		}
		budgetSummary.setBudgetSummaryVOs(budgetSummaryVOs);
		for (BudgetDetail budgetDetail : budgetDetails) {
			if (budgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
				costElements.add(budgetDetail.getCostElementCode());
			} else {
				budgetCategoryCodes.add(budgetDetail.getBudgetCategoryCode());
			}
		}
		BigDecimal periodTotalSum = BigDecimal.ZERO;
		BigDecimal periodCostShareTotalSum = BigDecimal.ZERO;
		BigDecimal periodTotalFundRequestedSum = BigDecimal.ZERO;
		for (String budgetCategoryCode : budgetCategoryCodes) {
			BudgetPeriodSummary budgetPeriodSummary = new BudgetPeriodSummary();
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			BigDecimal totalFundRequested = BigDecimal.ZERO;
			BigDecimal totalCostShareAmount = BigDecimal.ZERO;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			BigDecimal totalCostShareAmountSum = BigDecimal.ZERO;
			BigDecimal totalFundRequestedSum = BigDecimal.ZERO;
			List<BudgetSummaryVO> budgetSummaryVos = new ArrayList<>();
			BudgetCategory budgetCategory = budgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
			budgetPeriodSummary.setBudgetCategory(budgetCategory.getDescription());
			budgetPeriodSummary.setSortOrder(budgetCategory.getSortOrder());
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				totalFundRequested = BigDecimal.ZERO;
				totalCostShareAmount = BigDecimal.ZERO;
				totalUnderRecoveryAmount = BigDecimal.ZERO;
				for (BudgetDetail budgetDetail : budgetDetails) {
					if (budgetCategoryCode.equals(budgetDetail.getBudgetCategoryCode()) && (budgetPeriod.getBudgetPeriodId().equals(budgetDetail.getPeriod().getBudgetPeriodId())) && !budgetCategoryCode.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
						if (budgetDetail.getSponsorRequestedAmount()!=null) {
							   totalFundRequested = totalFundRequested.add(budgetDetail.getSponsorRequestedAmount());
							}
						if (budgetDetail.getLineItemCost()!=null) {
						   totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
						}
							if (budgetDetail.getCostSharingAmount() != null) {
								if (budgetDetail.getSystemGeneratedCEType() != null && budgetDetail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
									totalUnderRecoveryAmount = budgetPeriod.getUnderrecoveryAmount();
									totalCostShareAmount = totalCostShareAmount.add(totalUnderRecoveryAmount).add(budgetDetail.getCostSharingAmount());
								} else {
									totalCostShareAmount = totalCostShareAmount.add(budgetDetail.getCostSharingAmount());
								}
							}
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				totalCostShareAmountSum = totalCostShareAmountSum.add(totalCostShareAmount);
				totalFundRequestedSum = totalFundRequestedSum.add(totalFundRequested);
				BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				budgetSummaryVO.setTotalCostSharingAmount(totalCostShareAmount);
				budgetSummaryVO.setTotalFundRequested(totalFundRequested);
				budgetSummaryVos.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			periodCostShareTotalSum = periodCostShareTotalSum.add(totalCostShareAmountSum).setScale(2, RoundingMode.HALF_UP).setScale(2);
			periodTotalFundRequestedSum = periodTotalFundRequestedSum.add(totalFundRequestedSum);
			budgetPeriodSummary.setBudgetSummaryVOs(budgetSummaryVos);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalFundRequestedSum);
			budgetPeriodSummary.setTotalCostShareAmountSum(totalCostShareAmountSum);
			budgetPeriodSummary.setBudgetCategoryCode(budgetCategory.getCode());
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		for (String costElement : costElements) {
			BudgetPeriodSummary budgetPeriodSummary = new BudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalCostShareAmount;
			BigDecimal totalFundRequested = BigDecimal.ZERO;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			BigDecimal totalFundRequestedSum = BigDecimal.ZERO;
			BigDecimal totalCostShareAmountSum = BigDecimal.ZERO;
			List<BudgetSummaryVO> budgetSummaryVos = new ArrayList<>();
			budgetPeriodSummary.setBudgetCategory(budgetDao.fetchCostElementName(costElement));
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				totalUnderRecoveryAmount = BigDecimal.ZERO;
				totalLineItemCost = BigDecimal.ZERO;
				totalFundRequested = BigDecimal.ZERO;
				totalCostShareAmount = BigDecimal.ZERO;
				for (BudgetDetail budgetDetail : budgetDetails) {
					if (costElement.equals(budgetDetail.getCostElementCode()) && (budgetPeriod.getBudgetPeriodId().equals(budgetDetail.getPeriod().getBudgetPeriodId())) && budgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
							if (budgetDetail.getSponsorRequestedAmount() != null) {
								totalFundRequested = totalFundRequested.add(budgetDetail.getSponsorRequestedAmount());
							}
							if (budgetDetail.getLineItemCost() != null) {
								totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
							}
							if (budgetDetail.getCostSharingAmount() != null) {
								if (budgetDetail.getSystemGeneratedCEType() != null && budgetDetail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
									totalUnderRecoveryAmount = budgetPeriod.getUnderrecoveryAmount();
									totalCostShareAmount = totalCostShareAmount.add(totalUnderRecoveryAmount).add(budgetDetail.getCostSharingAmount());
								} else {
									totalCostShareAmount = totalCostShareAmount.add(budgetDetail.getCostSharingAmount());
								}
							}
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				totalCostShareAmountSum = totalCostShareAmountSum.add(totalCostShareAmount);
				totalFundRequestedSum = totalFundRequestedSum.add(totalFundRequested);
				BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				budgetSummaryVO.setTotalFundRequested(totalFundRequested);
				budgetSummaryVO.setTotalCostSharingAmount(totalCostShareAmount);
				budgetSummaryVos.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			periodTotalFundRequestedSum = periodTotalFundRequestedSum.add(totalFundRequestedSum);
			periodCostShareTotalSum = periodCostShareTotalSum.add(totalCostShareAmountSum).setScale(2, RoundingMode.HALF_UP).setScale(2);
			budgetPeriodSummary.setBudgetSummaryVOs(budgetSummaryVos);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalFundRequestedSum);
			budgetPeriodSummary.setTotalCostShareAmountSum(totalCostShareAmountSum);
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		budgetSummary.setBudgetPeriodSummaries(budgetPeriodSummaries);
		budgetSummary.setPeriodTotalSum(periodTotalSum);
		budgetSummary.setPeriodTotalCostShareSum(periodCostShareTotalSum);
		budgetSummary.setBudgetHeader(budgetHeader);
		budgetSummary.setPeriodTotalFundRequestedSum(periodTotalFundRequestedSum);
		return budgetSummary;
	}

	public List<BudgetPersonalDetails> copyBudgetPersonDetails(List<BudgetPersonalDetails> personDetails, BudgetDetail budgetdetail, BudgetPeriod currentPeriod) {
		List<BudgetPersonalDetails> copiedBudgetPersons = new ArrayList<>();
		for (BudgetPersonalDetails personDetail : personDetails) {
			BudgetPersonalDetails copiedBudgetPerson = new BudgetPersonalDetails();
			copiedBudgetPerson.setBudgetDetail(budgetdetail);
			copiedBudgetPerson.setPersonName(personDetail.getPersonName());
			copiedBudgetPerson.setPersonId(personDetail.getPersonId());
			copiedBudgetPerson.setPersonType(personDetail.getPersonType());
			copiedBudgetPerson.setRolodexId(personDetail.getRolodexId());
			copiedBudgetPerson.setTbnId(personDetail.getTbnId());
			copiedBudgetPerson.setUnderRecoveryAmount(personDetail.getUnderRecoveryAmount());
			copiedBudgetPerson.setPercentageCharged(personDetail.getPercentageCharged());
			copiedBudgetPerson.setPercentageEffort(personDetail.getPercentageEffort());
			copiedBudgetPerson.setCostSharingAmount(personDetail.getCostSharingAmount());
			copiedBudgetPerson.setCostSharingPercentage(personDetail.getCostSharingPercentage());
			copiedBudgetPerson.setSalaryRequested(personDetail.getSalaryRequested());
			copiedBudgetPerson.setTotalSalary(personDetail.getTotalSalary());
			copiedBudgetPerson.setNoOfMonths(personDetail.getNoOfMonths());
			copiedBudgetPerson.setUpdateTimeStamp(personDetail.getUpdateTimeStamp());
			copiedBudgetPerson.setUpdateUser(personDetail.getUpdateUser());
			copiedBudgetPerson.setEndDate(new Timestamp(currentPeriod.getEndDate().getTime()));
			copiedBudgetPerson.setStartDate(new Timestamp(currentPeriod.getStartDate().getTime()));
			copiedBudgetPerson.setBudgetPersonId(personDetail.getBudgetPersonId());
			copiedBudgetPerson.setSalary(personDetail.getSalary());
			copiedBudgetPerson.setSponsorRequestedAmount(personDetail.getSponsorRequestedAmount());
			copiedBudgetPerson.setBudgetPerson(personDetail.getBudgetPerson());
			copiedBudgetPersons.add(copiedBudgetPerson);
		}
		return copiedBudgetPersons;
	}

	@Override
	public String saveorUpdateBudgetSummary(BudgetVO vo) {
		return commonDao.convertObjectToJSON(budgetDao.saveBudgetHeader(vo.getBudgetHeader()));
	}
	
	// method to fetch budget parameter values.
	private void fetchBudgetParameterValues(BudgetVO vo) {
		vo.setIsSimpleBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_SIMPLE_BUDGET_ENABLED));
		vo.setIsModularBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_MODULAR_BUDGET_ENABLED));
		vo.setIsDetailedBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_DETAILED_BUDGET_ENABLED));
		vo.setIsAutoCalculateEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AUTO_CALCULATE));
		vo.setIsSysGeneratedCostElementEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT));
		vo.setIsSinglePeriodBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_BUDGET_ENABLE_SINGLE_PERIOD));
		vo.setIsShowCostShareAndUnderrecovery(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_COST_SHARE_AND_UNDERRECOVERY));
		vo.setIsShowInKind(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND));
		vo.setIsShowModifiedDirectCost(commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_MODIFIED_DIRECT_COST));
		vo.setIsBudgetVersionEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS));
		vo.setIsPeriodTotalEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_PERIODS_TOTAL_ENABLED));
		vo.setIsGeneratePeriodsEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_GENERATE_PERIODS_ENABLED));
		vo.setIsBudgetSummaryEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_SUMMARY_ENABLED));
		vo.setIsCampusFlagEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_PROPOSAL));
		vo.setIsOverHeadRateTypeEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_OVERHEAD_RATE_TYPE));
		vo.setShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
	}

	@Override
	public String deleteBudgetHeader(BudgetVO budgetVO) {
		Integer proposalId = budgetVO.getProposalId();
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetVO.getBudgetId());
		List<BudgetPerson> persons = budgetDao.getBudgetPersons(budgetVO.getBudgetId());
		budgetDao.deleteBudgetHeader(budgetHeader);
		if (persons != null && !persons.isEmpty()) {
			for (BudgetPerson budgetPerson : persons) {
				budgetDao.deleteBudgetPerson(budgetPerson.getBudgetPersonId());
			}
		}
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		BudgetHeader latestVersion = null;
		for (BudgetHeader budget : budgetHeaders) {
			if (budget.getIsLatestVersion()) {
				latestVersion = budget;
			}
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		if (latestVersion == null) {
			latestVersion = budgetDao.getMaxBudgetVersionOfBudget(proposalId);
			latestVersion.setIsLatestVersion(true);
			budgetDao.saveBudgetHeader(latestVersion);
		}
		latestVersion.setIsSelected(true);
		budgetVO.setBudgetHeader(latestVersion);
		budgetVO.setBudgetHeaderDetails(budgetHeaderDetails);
		budgetVO.setSimpleBudgetVo(prepareSimpleBudget(latestVersion.getBudgetId()));
		budgetVO.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		return commonDao.convertObjectToJSON(budgetVO);
	}

	@Override
	public String loadBudgetByProposalId(BudgetVO budgetVO) {
		fetchBudgetParameterValues(budgetVO);
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(budgetVO.getProposalId());
		Proposal proposal = proposalDao.fetchProposalById(budgetVO.getProposalId());
		budgetVO.setGrantTypeCode(proposal.getGrantTypeCode());
		if (proposal.getGrantCallType() != null) {
			budgetVO.setCategoryCode(proposal.getGrantCallType().getCategoryCode());
			budgetVO.setProposalType(proposal.getGrantCallType().getDescription());
		}
		budgetVO.setShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
		boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
		budgetVO.setEnableCostShareStatus(isEnableCostShareStatus);
		if(isEnableCostShareStatus) {
			budgetVO.setCostSharingType(budgetDao.getCostSharingType());
		}
		if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
			List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
			budgetVO.setIsBudgetHeaderFound(true);
			Boolean isApprovedBudget = false;
			Boolean isFinalBudget = false;
			for (BudgetHeader budgetHeader : budgetHeaders) {
				if (Boolean.TRUE.equals(budgetHeader.getIsApprovedBudget())) {
					isApprovedBudget = true;
					selectBudget(budgetHeader, budgetVO);
				} else if (Boolean.TRUE.equals(budgetHeader.getIsFinalBudget())) {
					if (!isApprovedBudget) {
						isFinalBudget = true;
						selectBudget(budgetHeader, budgetVO);
					}
				} else if (budgetHeader.getIsLatestVersion()) {
					if ((!isApprovedBudget) && (!isFinalBudget)) {
						selectBudget(budgetHeader, budgetVO);
					}
				}
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budgetHeader));
			}
			budgetVO.setBudgetHeaderDetails(budgetHeaderDetails);		
			if (commonDao.getParameterValueAsBoolean(Constants.COST_ELEMENT_FROM_TEMPLATE)) {
				budgetVO.setBudgetTemplateTypes(budgetDao.getBudgetTemplateTypesByModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE));
			}
		} else {
			budgetVO.setMessage("There is no budgets found");
		}
		return commonDao.convertObjectToJSON(budgetVO);
	}

	private void selectBudget(BudgetHeader budgetHeader, BudgetVO budgetVO) {
		budgetHeader.setIsSelected(true);
		Set<String> rateClassTypes = new HashSet<>();
		List<FibiProposalRate> proposalRates = budgetHeader.getProposalRates();
		if (proposalRates != null && !proposalRates.isEmpty()) {
			for (FibiProposalRate proposalRate : proposalRates) {
				rateClassTypes.add(proposalRate.getRateClass().getDescription());
			}
		}
		budgetVO.setRateClassTypes(rateClassTypes);
		loadBudgetInitialData(budgetVO);
		budgetVO.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		budgetVO.setBudgetHeader(budgetHeader);
		if (Boolean.TRUE.equals(budgetVO.getIsProposalComparison())) {
			budgetVO.setBudgetSummary(prepareBudgetSummary(budgetHeader));
			budgetVO.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
			budgetVO.setBudgetModularVO(budgetModularService.prepareModularBudget(budgetHeader.getBudgetId()));
		}
	}

	public boolean isBudgetActive(List<BudgetHeader> budgetHeaders) {
		boolean isSelected = false;
		for (BudgetHeader budgetHeader : budgetHeaders) {
			if (Boolean.TRUE.equals(budgetHeader.getIsSelected())) {
				isSelected = true;
			}
		}
		return isSelected;
	}

	private BudgetHeaderDetail prepareBudgetHeaderDetail(BudgetHeader budgetHeader) {
		BudgetHeaderDetail budgetHeaderDetail = new BudgetHeaderDetail();
		budgetHeaderDetail.setBudgetId(budgetHeader.getBudgetId());
		budgetHeaderDetail.setCostSharingAmount(budgetHeader.getCostSharingAmount());
		budgetHeaderDetail.setEndDate(budgetHeader.getEndDate());
		budgetHeaderDetail.setIsFinalBudget(budgetHeader.getIsFinalBudget());
		budgetHeaderDetail.setIsSelected(budgetHeader.getIsSelected());
		budgetHeaderDetail.setProposalId(budgetHeader.getProposalId());
		budgetHeaderDetail.setStartDate(budgetHeader.getStartDate());
		budgetHeaderDetail.setTotalCost(budgetHeader.getTotalCost());
		budgetHeaderDetail.setTotalDirectCost(budgetHeader.getTotalDirectCost());
		budgetHeaderDetail.setTotalIndirectCost(budgetHeader.getTotalIndirectCost());
		budgetHeaderDetail.setUnderrecoveryAmount(budgetHeader.getUnderrecoveryAmount());
		budgetHeaderDetail.setTotalFundRequested(budgetHeader.getTotalFundRequested());
		budgetHeaderDetail.setTotalModifiedDirectCost(budgetHeader.getTotalModifiedDirectCost());
		budgetHeaderDetail.setVersionNumber(budgetHeader.getVersionNumber());
		budgetHeaderDetail.setIsApprovedBudget(budgetHeader.getIsApprovedBudget());
		budgetHeaderDetail.setTotalInKind(budgetHeader.getTotalInKind());
		budgetHeaderDetail.setTotalModifiedDirectCost(budgetHeader.getTotalModifiedDirectCost());
		budgetHeaderDetail.setTotalOfTotalCost(budgetHeader.getTotalOfTotalCost());
		return budgetHeaderDetail;
	}

	@Override
	public String loadBudgetByBudgetId(BudgetVO budgetVO) {
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetVO.getBudgetId());
		fetchBudgetParameterValues(budgetVO);
		budgetHeader.setIsSelected(true);
		Set<String> rateClassTypes = new HashSet<>();
		List<FibiProposalRate> proposalRates = budgetHeader.getProposalRates();
		if (proposalRates != null && !proposalRates.isEmpty()) {
			for (FibiProposalRate proposalRate : proposalRates) {
				rateClassTypes.add(proposalRate.getRateClass().getDescription());
			}
		}
		budgetVO.setRateClassTypes(rateClassTypes);
		loadBudgetInitialData(budgetVO);
		budgetVO.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		budgetVO.setBudgetHeader(budgetHeader);
		budgetVO.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetVO.getBudgetId()));
		return commonDao.convertObjectToJSON(budgetVO);
	}

	public BudgetVO updateProposalBudget(BudgetVO vo) {
		Integer proposalId = vo.getProposalId();
		if (proposalId == null) {
			proposalId = vo.getBudgetHeader().getProposalId();
		}
		Integer budgetPeriod = vo.getBudgetPeriod();
		String activityTypeCode = vo.getActivityTypeCode();
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc()) && activityTypeCode != null) {
				calculate(budgetHeader, budgetPeriod, activityTypeCode);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		vo.setBudgetHeader(budgetHeader);
		return vo;
	}

	@Override
	public String loadSimpleBudgetByBudgetId(BudgetVO vo) {
		vo.setSimpleBudgetVo(prepareSimpleBudget(vo.getBudgetId()));
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
		vo.setTotalCost(budgetHeader.getTotalCost());
		vo.setTotalDirectCost(budgetHeader.getTotalCost());
		vo.setTotalIndirectCost(budgetHeader.getTotalIndirectCost());
		vo.setBudgetHeader(budgetHeader);
		vo.setProposalId(budgetHeader.getProposalId());
		vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(budgetHeader.getProposalId()));
		vo.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(vo.getGrantTypeCode()));
		Set<String> rateClassTypes = new HashSet<>();
		List<FibiProposalRate> proposalRates = budgetHeader.getProposalRates();
		if (proposalRates != null && !proposalRates.isEmpty()) {
			for (FibiProposalRate proposalRate : proposalRates) {
				rateClassTypes.add(proposalRate.getRateClass().getDescription());
			}
		}
		vo.setRateClassTypes(rateClassTypes);
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(vo.getBudgetId()));
		loadBudgetInitialData(vo);
		vo.setIsAutoCalculateEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AUTO_CALCULATE));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateSimpleBudget(BudgetVO vo) {
		Integer budgetPeriod = vo.getBudgetPeriod();
		String activityTypeCode = vo.getActivityTypeCode();
		List<SimpleBudgetVO> simpleBudgets = vo.getSimpleBudgetVo();
		for (SimpleBudgetVO simpleBudgetVO : simpleBudgets) {
			List<SimpleBudgetLineItemVO> lineItems = simpleBudgetVO.getLineItemList();
			for (SimpleBudgetLineItemVO lineItem : lineItems) {
				List<PeriodCost> periodCosts = lineItem.getPeriodCostsList();
				for (PeriodCost periodCost : periodCosts) {
					BudgetDetail budgetDetail = new BudgetDetail();
					if (periodCost.getBudgetDetailId() != null) {
						budgetDetail = budgetDao.fetchBudgetDetailBasedOnBudgetDetailId(periodCost.getBudgetDetailId());
					}
					budgetDetail.setBudgetPeriod(periodCost.getBudgetPeriod());
					budgetDetail.setPeriod(budgetDao.fetchBudgetPeriodBasedOnPeriodId(periodCost.getBudgetPeriodId()));
					budgetDetail.setLineItemNumber(lineItem.getLineItemNumber());
					budgetDetail.setCostElementCode(lineItem.getCostElementCode());
					budgetDetail.setCostElement(lineItem.getCostElement());
					budgetDetail.setBudgetCategoryCode(lineItem.getBudgetCategoryCode());
					budgetDetail.setBudgetCategory(lineItem.getBudgetCategory());
					budgetDetail.setLineItemDescription(lineItem.getLineItemDescription());
					budgetDetail.setLineItemCost(periodCost.getCost());
					budgetDetail.setIsSystemGeneratedCostElement(lineItem.getIsSystemGeneratedCostElement());
					budgetDetail.setSystemGeneratedCEType(lineItem.getSystemGeneratedCEType());
					budgetDetail.setUpdateUser(vo.getUpdateUser());
					budgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					budgetDao.saveBudgetDetail(budgetDetail);
				}
			}
		}
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
		budgetHeader.setIsAutoCalc(vo.getIsAutoCalc());
		if (Boolean.TRUE.equals(vo.getIsAutoCalc()) && activityTypeCode != null) {
				calculate(budgetHeader, budgetPeriod, activityTypeCode);
		}
		budgetDao.saveBudgetHeader(budgetHeader);
		return commonDao.convertObjectToJSON(prepareSimpleBudget(budgetHeader.getBudgetId()));
	}

	@Override
	public List<SimpleBudgetVO> prepareSimpleBudget(Integer budgetId) {
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetId);
		List<SimpleBudgetVO> simpleBudgets = new ArrayList<>();
		Set<String> budgetCategories = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		Set<Integer> lineItemNumbers = new HashSet<>();
		Set<Integer> updatedLineItemNumbers = lineItemNumbers;
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
				if (budgetDetails != null && !budgetDetails.isEmpty()) {
					for (BudgetDetail budgetDetail : budgetDetails) {
						budgetCategories.add(budgetDetail.getBudgetCategoryCode());
						costElements.add(budgetDetail.getCostElementCode());
						lineItemNumbers.add(budgetDetail.getLineItemNumber());
					}
				}
			}
		}
		for (String budgetCategory : budgetCategories) {
			SimpleBudgetVO simpleBudgetVO = new SimpleBudgetVO();
			BigDecimal totalCategoryCost = BigDecimal.ZERO;
			Boolean isUniquePeriodValues = false;
			Boolean removeLineItemNumber = false;
			simpleBudgetVO.setCategoryCode(budgetCategory);
			BudgetCategory budgetCategoryData = budgetDao.fetchBudgetCategoryBasedOnCode(budgetCategory);
			simpleBudgetVO.setCategoryName(budgetCategoryData.getDescription());
			simpleBudgetVO.setSortOrder(budgetCategoryData.getSortOrder());
			if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
				for (BudgetPeriod budgetPeriod : budgetPeriods) {
					List<SimpleBudgetLineItemVO> lineItems = new ArrayList<>();
					List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
					if (budgetDetails != null && !budgetDetails.isEmpty()) {
						for (BudgetDetail budgetDetail : budgetDetails) {
							if (budgetCategory.equals(budgetDetail.getBudgetCategoryCode())) {
								for (String costElement : costElements) {
									for (Integer lineItemNumber : lineItemNumbers) {
										if (costElement.equals(budgetDetail.getCostElementCode())&& (lineItemNumber.equals(budgetDetail.getLineItemNumber()))) {
											SimpleBudgetLineItemVO lineItem = new SimpleBudgetLineItemVO();
											lineItem.setQuantity(budgetDetail.getQuantity());
											lineItem.setLineItemDescription(budgetDetail.getLineItemDescription());
											lineItem.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
											lineItem.setBudgetCategory(budgetDetail.getBudgetCategory());
											lineItem.setCostElementCode(budgetDetail.getCostElementCode());
											lineItem.setCostElement(budgetDetail.getCostElement());
											lineItem.setLineItemNumber(budgetDetail.getLineItemNumber());
											lineItem.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
											lineItem.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
											lineItem.setTotalLineItemCost(calculateLineItemCost(costElement, budgetHeader, lineItemNumber));
											SimpleBudgetLineItemVO resultPeriod = preparePeriodCostList(costElement, budgetHeader, lineItemNumber);
											lineItem.setPeriodCostsList(resultPeriod.getPeriodCostsList());
											simpleBudgetVO.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
											if (Boolean.TRUE.equals(resultPeriod.getEmptyEntry()) && !budgetCategory.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
//												simpleBudgetVO.getLineItemList().clear();
												simpleBudgetVO.getLineItemList().add(lineItem);
											} else {
												isUniquePeriodValues = true;
												lineItems.add(lineItem);
											}
										}
									}
								}
								if (budgetDetail.getSponsorRequestedAmount()!=null) {
								totalCategoryCost = totalCategoryCost.add(budgetDetail.getSponsorRequestedAmount());
								}
							}
						}
					}
					if (Boolean.TRUE.equals(isUniquePeriodValues)) {
						Set<Integer> deleteLineItemNumbers = new HashSet<Integer>();
						if (simpleBudgetVO.getLineItemList() != null) {
							for (SimpleBudgetLineItemVO lineItemNumber : lineItems) {
								for (Integer updatedLineItemNumber : updatedLineItemNumbers) {
									if (lineItemNumber.getLineItemNumber().equals(updatedLineItemNumber)) {
										deleteLineItemNumbers.add(updatedLineItemNumber);
										removeLineItemNumber = true;
									}
								}
							}
							if (Boolean.TRUE.equals(removeLineItemNumber)) {
								simpleBudgetVO.getLineItemList().addAll(lineItems);
								for (Integer deleteLineItemNumber : deleteLineItemNumbers) {
									updatedLineItemNumbers.remove(deleteLineItemNumber);
								}
							}
						} else {
							simpleBudgetVO.setLineItemList(lineItems);
						}
					}
				}
			}
			simpleBudgetVO.setTotalCategoryCost(totalCategoryCost);
			simpleBudgets.add(simpleBudgetVO);
		}
		return simpleBudgets;
	}

	private BigDecimal calculateLineItemCost(String costElement, BudgetHeader budgetHeader, Integer lineItemNumber) {
		BigDecimal totalLineItemCost = BigDecimal.ZERO;
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
				if (budgetDetails != null && !budgetDetails.isEmpty()) {
					for (BudgetDetail budgetDetail : budgetDetails) {
						if (budgetDetail.getCostElementCode().equals(costElement) && (lineItemNumber.equals(budgetDetail.getLineItemNumber()))) {
							if (budgetDetail.getSponsorRequestedAmount() != null) {
								totalLineItemCost = totalLineItemCost.add(budgetDetail.getSponsorRequestedAmount());
							}
						}
					}
				}
			}
		}
		return totalLineItemCost;
	}

	private SimpleBudgetLineItemVO preparePeriodCostList(String costElement, BudgetHeader budgetHeader, Integer lineItemNumber) {
		SimpleBudgetLineItemVO simpleBudgetLineItemVO = new SimpleBudgetLineItemVO();
		List<PeriodCost> periodCosts = new ArrayList<>();
		Set<Integer> budgetPeriodIds = new HashSet<>();
		Set<Integer> budgetPeriodValue = new HashSet<>();
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				budgetPeriodIds.add(budgetPeriod.getBudgetPeriodId());
				budgetPeriodValue.add(budgetPeriod.getBudgetPeriod());
				List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
				if (budgetDetails != null && !budgetDetails.isEmpty()) {
					for (BudgetDetail budgetDetail : budgetDetails) {
						if (budgetDetail.getCostElementCode().equals(costElement) && (lineItemNumber == budgetDetail.getLineItemNumber())) {
							PeriodCost periodCost = new PeriodCost();
							periodCost.setBudgetDetailId(budgetDetail.getBudgetDetailId());
							periodCost.setBudgetPeriod(budgetDetail.getBudgetPeriod());
							if (budgetDetail.getSponsorRequestedAmount() != null) {
								periodCost.setCost(budgetDetail.getSponsorRequestedAmount());
							}
							periodCost.setBudgetPeriodId(budgetDetail.getPeriod().getBudgetPeriodId());
							periodCosts.add(periodCost);
							budgetPeriodValue.remove(budgetDetail.getBudgetPeriod());
						}
					}
				}
			}
		}
		List<PeriodCost> updatedPeriodCosts = new ArrayList<PeriodCost>(periodCosts);
		Collections.copy(updatedPeriodCosts, periodCosts);
		Set<Integer> budgetPeriodCostListIds = new HashSet<>();
		for (PeriodCost periodCost : periodCosts) {
			budgetPeriodCostListIds.add(periodCost.getBudgetPeriodId());
		}
		if (periodCosts.size() != budgetPeriods.size()) {
			simpleBudgetLineItemVO.setEmptyEntry(true);
			Integer differSize = budgetPeriodIds.size() - budgetPeriodCostListIds.size();
			List<Integer> budgetPeriodsIds = new ArrayList<>(budgetPeriodValue);
			for (Integer i = 0; i < differSize; i++) {
				PeriodCost periodCost = new PeriodCost();
				periodCost.setBudgetPeriod(budgetPeriodsIds.get(i));
				updatedPeriodCosts.add(periodCost);
			}
		}
		simpleBudgetLineItemVO.setPeriodCostsList(updatedPeriodCosts);
		return simpleBudgetLineItemVO;
	}

	@Override
	public String generateSimpleBudgetPeriods(BudgetVO vo) {
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		budgetDao.saveBudgetHeader(budgetHeader);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		BudgetPeriod copyPeriod = budgetDao.getPeriodById(vo.getCurrentPeriodId());
		for (BudgetPeriod currentPeriod : budgetPeriods) {
			List<BudgetDetail> details = new ArrayList<BudgetDetail>(currentPeriod.getBudgetDetails());
			Collections.copy(details, currentPeriod.getBudgetDetails());
			if (!currentPeriod.getBudgetPeriodId().equals(vo.getCurrentPeriodId())) {
				currentPeriod.setGeneratePeriod(true);
				copySimpleBudgetDetails(copyPeriod, currentPeriod, vo.getUserName());
				budgetDao.deleteAllBudgetDetail(details);
			}
		}
		if (budgetHeader.getIsAutoCalc() != null && !budgetHeader.getIsAutoCalc()) {
			budgetHeader = calculateCost(budgetHeader);
		} else {
			updateProposalBudget(vo);
		}
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setSimpleBudgetVo(prepareSimpleBudget(budgetHeader.getBudgetId()));
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String createApprovedProposalBudget(BudgetVO vo) {
		Integer proposalId = vo.getProposalId();
		BudgetHeader orginalBudget = null;
		if (vo.getBudgetId() != null) {
			orginalBudget = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
			BudgetHeader copyBudget = new BudgetHeader();
			copyBudget.setIsApprovedBudget(true);
			Integer budgetVersionNumber = budgetDao.maxBudgetVersionNumberByProposalId(proposalId);// .intValue() + 1;
			if (budgetVersionNumber == null) {
				budgetVersionNumber = 1;
			} else {
				budgetVersionNumber = budgetVersionNumber + 1;
			}
			copyBudget.setProposalId(orginalBudget.getProposalId());
			createBudgetHeader(copyBudget, orginalBudget,vo.getUserName(), vo.getUserFullName(), budgetVersionNumber, Constants.COPY_APPROVED_BUDGET);
			copyBudget.setComments(vo.getBudgetDescription());
			Set<String> rateClassTypes = new HashSet<>();
			Integer grantTypeCode = vo.getGrantTypeCode();
			copyBudget.setProposalRates(fetchFilteredProposalRates(copyBudget, vo.getActivityTypeCode(), rateClassTypes, grantTypeCode));
			vo.setRateClassTypes(rateClassTypes);
			if (orginalBudget.getBudgetPeriods() != null && !orginalBudget.getBudgetPeriods().isEmpty()) {
				budgetDao.saveBudgetHeader(copyBudget);
				copyBudget.getBudgetPeriods().addAll(proposalCopyService.copyBudgetPeriods(copyBudget, orginalBudget, vo.getActivityTypeCode(), vo.getUserName()));
			}
			if (copyBudget.getIsAutoCalc() != null && !copyBudget.getIsAutoCalc()) {
				copyBudget = calculateCost(copyBudget);
			} else {
				// updateProposalBudget(vo);
				updateBudgetHeader(copyBudget);
			}
			budgetDao.saveBudgetHeader(copyBudget);
			vo.setBudgetHeader(copyBudget);
			vo.setGrantTypeCode(proposalDao.fetchGrantTypeCodeBasedOnProposalId(copyBudget.getProposalId()));
			List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(vo.getProposalId());
			List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
			for (BudgetHeader budget : budgetHeaders) {
				budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
			}
			vo.setBudgetHeaderDetails(budgetHeaderDetails);
			vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(copyBudget.getBudgetId()));
			fetchBudgetParameterValues(vo);
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			vo.setGrantTypeCode(grantTypeCode);
			vo.setProposalType(proposal.getGrantCallType() != null ? proposal.getGrantCallType().getDescription() : null);
			return commonDao.convertObjectToJSON(vo);
		} else {
			vo.setIsFirstVersion(true);
			vo.setIsApprovedBudget(true);
			return createProposalBudget(vo);
		}
	}

	public Integer fetchSysGeneratedCESize(String activityTypeCode) {
		List<String> systemGeneratedCE = new ArrayList<>();
		if (activityTypeCode != null && activityTypeCode.equals(Constants.ACTIVITY_TYPE_CODE_RESEARCH)) {
			systemGeneratedCE.add(commonDao.getParameterValueAsString(Constants.BUDGET_RESEARCH_OH_ON));
		} else {
			systemGeneratedCE.add(commonDao.getParameterValueAsString(Constants.BUDGET_OH_ON));
		}
		String fringeCostElement = commonDao.getParameterValueAsString(Constants.BUDGET_FRINGE_ON);
		if (fringeCostElement != null && !fringeCostElement.isEmpty()) {
			systemGeneratedCE.add(fringeCostElement);
		}
		return systemGeneratedCE.size();
	}

	@Override
	public BudgetVO convertStringJSONToObject(String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		BudgetVO vo = null;
		try {
			mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
			vo = mapper.readValue(formDataJSON, BudgetVO.class);
		} catch (JsonParseException e) {
			e.printStackTrace();
		} catch (JsonMappingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return vo;
	}
	
	@Override
	public BudgetHeader calculateBudgetByPeriod(BudgetHeader budgetHeader, String activityTypeCode) {
		BudgetHeader budgetHeaders = new BudgetHeader();
		budgetHeaders = calculateBudgetDetails(budgetHeader, activityTypeCode);
		return budgetHeaders;
	}

	@Override
	public String fetchCostElementByCategories() {
		if (commonDao.getParameterValueAsBoolean(Constants.BUD_CAT_TOTAL_BY_BUD_CAT)) {
			return commonDao.convertObjectToJSON(budgetDao.fetchCostElementNotInBudgetCategoryCode(null));
		} else {
			return commonDao.convertObjectToJSON(budgetDao
					.fetchCostElementNotInBudgetCategoryCode(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL));
		}
	}

	@Override
	public void clearSystemGenCost(BudgetVO vo) {
		BudgetPeriod period = budgetDao.getPeriodById(vo.getBudgetPeriodId());
		List<BudgetDetail> budgetDetails = period.getBudgetDetails();
		List<BudgetDetail> updatedBudgetDetails = new ArrayList<BudgetDetail>(budgetDetails);
		Collections.copy(updatedBudgetDetails, budgetDetails);
		for (BudgetDetail budgetDetail : budgetDetails) {
			if (vo.getBudgetPeriod().equals(budgetDetail.getBudgetPeriod())) {
				if (Boolean.TRUE.equals(budgetDetail.getIsSystemGeneratedCostElement()) && updatedBudgetDetails.size() <= 2) {
					budgetDetail = budgetDao.deleteBudgetDetail(budgetDetail);
					updatedBudgetDetails.remove(budgetDetail);

				}
			}
		}
		period.getBudgetDetails().clear();
		period.getBudgetDetails().addAll(updatedBudgetDetails);
		budgetDao.saveBudgetPeriod(period);
	}

	@Override
	public String calculateAfterDelete(BudgetVO vo) {
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(vo.getBudgetHeader().getBudgetId());
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			budgetDao.saveBudgetHeader(calculateBudgetDetails(budgetHeader, vo.getActivityTypeCode()));
		}else {
			calculateByPeriodCost(budgetHeader);
		}
		vo.setBudgetHeader(budgetHeader);
		vo.setActivityTypeCode(vo.getActivityTypeCode());
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(budgetHeader.getProposalId());
		List<BudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (BudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareBudgetHeaderDetail(budget));
		}
		vo.setMaxLineItemNumber(budgetDao.maxBudgetLineItemNumberByBudgetHeader(budgetHeader.getBudgetId()));
		vo.setBudgetHeaderDetails(budgetHeaderDetails);
		vo.setGrantTypeCode(vo.getGrantTypeCode());
		vo.setCategoryCode(grantCallDao.fetchGrantCategoryCodeByGrantTypeCode(vo.getGrantTypeCode()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<BudgetPeriod> generateExtendedBudgetPeriods(Proposal proposal, String userName) {
		List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposal.getProposalId());
		List<BudgetPeriod> budgetPeriods = new ArrayList<BudgetPeriod>();
		if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
			for (BudgetHeader budgetHeader : budgetHeaders) {
				if (budgetHeader.getIsLatestVersion()) {
					if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_SINGLE_PERIOD_BUDGET)) {
						BudgetPeriod lastPeriod = budgetDao.getMaxBudgetPeriodByBudgetId(budgetHeader.getBudgetId());
						Calendar c = Calendar.getInstance();
						c.setTime(budgetHeader.getEndDate());
						c.add(Calendar.DAY_OF_YEAR, 1);
						Date projectEndDate = new Date(proposal.getEndDate().getTime());
						Date projectStartDate = new Date(proposal.getStartDate().getTime());
						boolean budgetPeriodExists = true;
						Calendar cl = Calendar.getInstance();
						int budgetPeriodNum = lastPeriod.getBudgetPeriod();
						if (budgetPeriodExists) {
							cl.setTime(projectStartDate);
							cl.add(Calendar.YEAR, 1);
							lastPeriod.setBudgetPeriod(budgetPeriodNum);
							Timestamp periodStartDateTimeStamp = new Timestamp(projectStartDate.getTime());
							Timestamp periodEndDateTimeStamp = new Timestamp(projectEndDate.getTime());
							lastPeriod.setStartDate(periodStartDateTimeStamp);
							lastPeriod.setEndDate(periodEndDateTimeStamp);
							lastPeriod.setBudget(budgetHeader);
							budgetPeriods.add(lastPeriod);
						}
					} else {
						BudgetPeriod lastPeriod = budgetDao.getMaxBudgetPeriodByBudgetId(budgetHeader.getBudgetId());
						Calendar c = Calendar.getInstance();
						c.setTime(budgetHeader.getEndDate());
						c.add(Calendar.DAY_OF_YEAR, 1);
						Date newBudgetPeriodStartDate = c.getTime();
						Date projectEndDate = new Date(proposal.getEndDate().getTime());
						boolean budgetPeriodExists = true;
						Calendar cl = Calendar.getInstance();
						Date periodStartDate = newBudgetPeriodStartDate;
						int budgetPeriodNum = lastPeriod.getBudgetPeriod() + 1;
						int budgetPeriodEndingYear = getYear(projectEndDate);
						if (isPeriodGenerationBasedOnCalenderYear()) {
							while (budgetPeriodExists) {
								cl.setTime(periodStartDate);
								cl.add(Calendar.DATE, 1);
								Date periodEndDate = new Date(cl.getTime().getTime());
								int currentPeriodYear = cl.get(Calendar.YEAR);
								if (currentPeriodYear < budgetPeriodEndingYear) {
									periodEndDate = getEndDateForaYear(currentPeriodYear);
								} else {
									periodEndDate = projectEndDate;
								}
								cl.setTime(periodEndDate);
								Date nextPeriodStartDate = new Date(cl.getTime().getTime());
								/* check period end date gt project end date */
								switch (periodEndDate.compareTo(projectEndDate)) {
								case 1:
									periodEndDate = projectEndDate;
									// the break statement is purposefully missing.
								case 0:
									budgetPeriodExists = false;
									break;
								}
								BudgetPeriod budgetPeriod = new BudgetPeriod();
								budgetPeriod.setBudgetPeriod(budgetPeriodNum);
								Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
								Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
								budgetPeriod.setStartDate(periodStartDateTimeStamp);
								budgetPeriod.setEndDate(periodEndDateTimeStamp);
								budgetPeriod.setBudget(budgetHeader);
								budgetPeriod.setTotalCost(BigDecimal.ZERO);
								budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
								budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
								budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
								budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
								budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
								budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
								budgetPeriods.add(budgetPeriod);
								cl.setTime(new Timestamp(nextPeriodStartDate.getTime()));
								cl.add(Calendar.DATE, 1);
								periodStartDate = new Date(cl.getTime().getTime());
								budgetPeriodNum++;
							}
						} else {
							while (budgetPeriodExists) {
								cl.setTime(periodStartDate);
								cl.add(Calendar.YEAR, 1);
								Date nextPeriodStartDate = new Date(cl.getTime().getTime());
								cl.add(Calendar.DATE, -1);
								Date periodEndDate = new Date(cl.getTime().getTime());
								/* check period end date gt project end date */
								switch (periodEndDate.compareTo(projectEndDate)) {
								case 1:
									periodEndDate = projectEndDate;
									// the break statement is purposefully missing.
								case 0:
									budgetPeriodExists = false;
									break;
								}
								BudgetPeriod budgetPeriod = new BudgetPeriod();
								budgetPeriod.setBudgetPeriod(budgetPeriodNum);
								Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
								Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
								budgetPeriod.setStartDate(periodStartDateTimeStamp);
								budgetPeriod.setEndDate(periodEndDateTimeStamp);
								budgetPeriod.setBudget(budgetHeader);
								copySimpleBudgetDetails(lastPeriod, budgetPeriod, userName);
								budgetPeriods.add(budgetPeriod);
								periodStartDate = nextPeriodStartDate;
								budgetPeriodNum++;
							}
						}
					}
				}
			}
		}
		return budgetPeriods;
	}

	@Override
	public String updateProposalBudgetPeriods(BudgetVO vo) {
		List<BudgetPeriod> budgetPeriods = new ArrayList<>();
		BudgetHeader budgetHeader = null;
		Proposal proposal = proposalDao.fetchProposalById(vo.getProposalId());
		if (vo.getBudgetId() != null) {
			budgetHeader = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
		} else {
			budgetHeader = budgetDao.fetchFinalBudget(vo.getProposalId());
			if (Boolean.FALSE.equals(budgetHeader.getIsFinalBudget())) {
				budgetHeader = budgetDao.getMaxBudgetVersionOfBudget(vo.getProposalId());
			}
		}
		Date previousEndDate = new Date(budgetHeader.getEndDate().getTime());
		Date previousStartDate = new Date(budgetHeader.getStartDate().getTime());
		budgetHeader.setStartDate(proposal.getStartDate());
		budgetHeader.setEndDate(proposal.getEndDate());
		Date endDate = new Date(budgetHeader.getEndDate().getTime());
		Date startDate = new Date(budgetHeader.getStartDate().getTime());
		if (!(startDate.compareTo(previousStartDate) == 0 && endDate.compareTo(previousEndDate) == 0)) {
			int newEndDateYear = getYear(endDate);
			int newStartYear = getYear(startDate);
			if (commonDao.getParameterValueAsBoolean(Constants.PROPOSAL_BUDGET_ENABLE_SINGLE_PERIOD)) {
				setSinglePeriods(budgetPeriods, budgetHeader, startDate, endDate);
			} else {
				prepareBudgetPeriods(budgetPeriods, newStartYear, newEndDateYear, budgetHeader, startDate, endDate);
			}
			budgetHeader.getBudgetPeriods().clear();
			budgetHeader.getBudgetPeriods().addAll(budgetPeriods);
			String activityTypeCode = vo.getActivityTypeCode();
			if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
				if (!budgetHeader.getProposalRates().isEmpty()) {
					proposalDao.deleteProposalBudgetRate(budgetHeader.getProposalRates());
				}
				budgetHeader.getProposalRates().clear();
				budgetDao.saveBudgetHeader(budgetHeader);
				Set<String> rateClassTypes = new HashSet<>();
				Integer grantTypeCode = proposal.getGrantTypeCode();
				List<FibiProposalRate> proposalRates = fetchFilteredProposalRates(budgetHeader, activityTypeCode, rateClassTypes, grantTypeCode);
				if (proposalRates != null && !proposalRates.isEmpty()) {
					for (FibiProposalRate proposalRate : proposalRates) {
						rateClassTypes.add(proposalRate.getRateClass().getDescription());
					}
				}
				vo.setRateClassTypes(rateClassTypes);
				budgetHeader.getProposalRates().addAll(proposalRates);
				budgetDao.saveBudgetHeader(budgetHeader);
				calculate(budgetHeader, null, activityTypeCode);
			} else {
				calculateByPeriodCost(budgetHeader);
			}
			budgetHeader = budgetDao.saveBudgetHeader(budgetHeader);
		}
		vo.setBudgetHeader(budgetHeader);
		fetchBudgetParameterValues(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private List<BudgetPeriod> prepareBudgetPeriods(List<BudgetPeriod> budgetPeriods, int newStartYear, int newEndDateYear,
			BudgetHeader budgetHeader, Date startDate, Date endDate){
		if (isPeriodGenerationBasedOnCalenderYear()) {
			setBudgetPeriodByCalendarYear(budgetPeriods, newStartYear, newEndDateYear, budgetHeader, startDate, endDate);
		} else {
			setBudgetPeriods(budgetPeriods, newStartYear, newEndDateYear, budgetHeader, startDate, endDate);
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> setBudgetPeriods(List<BudgetPeriod> budgetPeriods, int newStartYear, int newEndDateYear,
			BudgetHeader budgetHeader, Date startDate, Date endDate) {
		if (newStartYear == newEndDateYear) {
			setSinglePeriods(budgetPeriods, budgetHeader, startDate, endDate);
		} else {
			budgetPeriods = prepareBudgetDates(budgetPeriods, budgetHeader, startDate, endDate, newStartYear);
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> setBudgetPeriodByCalendarYear(List<BudgetPeriod> budgetPeriods, int newStartYear, int newEndDateYear, BudgetHeader budgetHeader, Date startDate, Date endDate ) {
		if (newStartYear == newEndDateYear) {
			setSinglePeriods(budgetPeriods, budgetHeader, startDate, endDate);
		} else {
			budgetPeriods = prepareBudgetDatesBasedOnCalenderYear(budgetPeriods, budgetHeader, startDate, endDate, newStartYear);
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareSingleBudgetBeriodByCalenderYear(Date periodStartDate, Date periodEndDate,
			List<BudgetPeriod> budgetPeriods, BudgetPeriod budgetPeriod, BudgetHeader budget) {
		if (periodStartDate.compareTo(new Date(budget.getStartDate().getTime())) <= 0) {
			periodStartDate = budget.getStartDate();
		}
		if (periodEndDate.compareTo(new Date(budget.getEndDate().getTime())) >= 0) {
			periodEndDate = budget.getEndDate();
		}

		Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
		budgetPeriod.setEndDate(periodEndDateTimeStamp);
		Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
		budgetPeriod.setStartDate(periodStartDateTimeStamp);
		budgetPeriod.setBudget(budget);
		budgetPeriods.add(budgetPeriod);
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareBudgetPeriods(List<BudgetPeriod> budgetPeriods, Date projectStartDate,
			BudgetHeader budget) {
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;
		Calendar clndr = Calendar.getInstance();
		Date periodStartDate = projectStartDate;
		Integer budgetPeriodNum = getPeriodNumber(budgetPeriods);
		while (budgetPeriodExists) {
			clndr.setTime(periodStartDate);
			clndr.add(Calendar.YEAR, 1);
			Date nextPeriodStartDate = new Date(clndr.getTime().getTime());
			clndr.add(Calendar.DATE, -1);
			Date periodEndDate = new Date(clndr.getTime().getTime());
			/* check period end date gt project end date */
			switch (periodEndDate.compareTo(projectEndDate)) {
			case 1:
				periodEndDate = projectEndDate;
				// the break statement is purposefully missing.
			case 0:
				budgetPeriodExists = false;
				break;
			}
			BudgetPeriod budgetPeriod = new BudgetPeriod();
			budgetPeriod.setTotalCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
			budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
			budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
			budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
			budgetPeriod.setBudgetPeriod(budgetPeriodNum);
			Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
			Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
			budgetPeriod.setStartDate(periodStartDateTimeStamp);
			budgetPeriod.setEndDate(periodEndDateTimeStamp);
			budgetPeriod.setBudget(budget);
			budgetPeriods.add(budgetPeriod);
			periodStartDate = nextPeriodStartDate;
			budgetPeriodNum++;
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareBudgetPeriodsByCalendarYear(List<BudgetPeriod> budgetPeriods,	Date projectStartDate, BudgetHeader budget) {
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;
		int budgetPeriodEndingYear = getYear(projectEndDate);
		Calendar cl = Calendar.getInstance();
		Date periodStartDate = getLastPeriodStartDate(budgetPeriods, projectStartDate, cl);
		Integer budgetPeriodNum = getPeriodNumber(budgetPeriods);
		while (budgetPeriodExists) {
			cl.setTime(periodStartDate);
			cl.add(Calendar.DATE, 1);
			Date periodEndDate = new Date(cl.getTime().getTime());
			int currentPeriodYear = cl.get(Calendar.YEAR);
			if (currentPeriodYear < budgetPeriodEndingYear) {
				periodEndDate = getEndDateForaYear(currentPeriodYear);
			} else {
				periodEndDate = projectEndDate;
			}
			cl.setTime(periodEndDate);
			Date nextPeriodStartDate = new Date(cl.getTime().getTime());
			switch (periodEndDate.compareTo(projectEndDate)) {
			case 1:
				periodEndDate = projectEndDate;
				// the break statement is purposefully missing.
			case 0:
				budgetPeriodExists = false;
				break;
			}
			BudgetPeriod budgetPeriod = new BudgetPeriod();
			budgetPeriod.setBudgetPeriod(budgetPeriodNum);
			Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
			Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
			budgetPeriod.setStartDate(periodStartDateTimeStamp);
			budgetPeriod.setEndDate(periodEndDateTimeStamp);
			budgetPeriod.setBudget(budget);
			budgetPeriod.setTotalCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCostLimit(BigDecimal.ZERO);
			budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalCostLimit(BigDecimal.ZERO);
			budgetPeriod.setUnderrecoveryAmount(BigDecimal.ZERO);
			budgetPeriod.setCostSharingAmount(BigDecimal.ZERO);
			budgetPeriods.add(budgetPeriod);
			cl.setTime(new Timestamp(nextPeriodStartDate.getTime()));
			cl.add(Calendar.DATE, 1);
			periodStartDate = new Date(cl.getTime().getTime());
			budgetPeriodNum++;
		}
		return budgetPeriods;
	}

	private Integer getPeriodNumber(List<BudgetPeriod> budgetPeriods) {
		Integer budgetPeriodNum = 0;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			budgetPeriodNum = budgetPeriods.get(0).getBudgetPeriod() > 1 ? budgetPeriods.get(budgetPeriods.size() - 1).getBudgetPeriod() + 1
					: budgetPeriods.size() + 1;
		}
		if (budgetPeriodNum == 0) {
			budgetPeriodNum = budgetPeriods != null && !budgetPeriods.isEmpty() ? budgetPeriods.size() + 1 : 1;
		}
		return budgetPeriodNum;
	}

	private Date getLastPeriodStartDate(List<BudgetPeriod> budgetPeriods, Date projectStartDate, Calendar cl) {
		Date periodStartDate;
		Integer lastPeriodStartDate = getYear(new Date(budgetPeriods.get(budgetPeriods.size() - 1).getStartDate().getTime()));
		Integer lastPeriodEndDate = getYear(new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime()));
		if (lastPeriodEndDate - lastPeriodStartDate != 0) {
			periodStartDate = new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime());
			cl.setTime(periodStartDate);
			cl.add(Calendar.YEAR, 1);
			periodStartDate = getStartDateForaYear(getYear(new Date(cl.getTime().getTime())));
		} else {
			periodStartDate = projectStartDate;
		}
		return periodStartDate;
	}

	private List<BudgetPeriod> setSinglePeriods(List<BudgetPeriod> budgetPeriods, BudgetHeader budget, Date projectStartDate, Date projectEndDate) {
		budget.getBudgetPeriods().get(0).setStartDate( new Timestamp(projectStartDate.getTime()));
		budget.getBudgetPeriods().get(0).setEndDate( new Timestamp(projectEndDate.getTime()));
		budgetPeriods.addAll(budget.getBudgetPeriods());
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareBudgetPeriodsBasedOnCalenderYear(List<BudgetPeriod> budgetPeriods, Calendar cl,
			BudgetHeader budget, Date projectStartDate, Date projectEndDate, int newStartYear) {
		List<BudgetPeriod> budgetPeriodDetails = new ArrayList<>();
		Integer count = 0;
		Calendar calendar = Calendar.getInstance();
		List<BudgetPeriod> periods = budget.getBudgetPeriods().stream().sorted(Comparator.comparing(BudgetPeriod::getBudgetPeriod)).collect(Collectors.toList());
		Calendar cal = Calendar.getInstance();
		cal.setTime(new Date(budget.getStartDate().getTime()));
		for (BudgetPeriod budgetPeriod : periods) {
			if (count < 2) {
				Date periodStartDate = projectStartDate;
				cl.setTime(periodStartDate);
				Date periodEndDate = getEndDateForPeriod(cl, projectEndDate, periodStartDate);
				Date nextPeriodDate = periodStartDate;
				cl.setTime(periodEndDate);
				if (periodEndDate.compareTo(projectEndDate) == 0) {
					count++;
				}
				if (periodEndDate.compareTo(projectEndDate) >= 0) {
					if (count == 1) {
						Timestamp periodEndDateTimeStamp = new Timestamp(projectEndDate.getTime());
						budgetPeriod.setEndDate(periodEndDateTimeStamp);
						Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
						budgetPeriod.setStartDate(periodStartDateTimeStamp);
						budgetPeriod.setBudget(budget);
						budgetPeriods.add(budgetPeriod);
						count++;
					}
				} else {
					prepareSingleBudgetBeriodByCalenderYear(periodStartDate, periodEndDate, budgetPeriods, budgetPeriod,
							budget);
				}
				if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
					newStartYear = getYear(
							new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime()));
				}
				newStartYear++;
				if (!budgetPeriods.isEmpty()) {
					nextPeriodDate = new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime());
					cl.setTime(nextPeriodDate);
					cl.add(Calendar.YEAR, 1);
					projectStartDate = getStartDateForaYear(getYear(new Date(cl.getTime().getTime())));
				}
			} else {
				budgetPeriod.setBudget(budget);
				budgetPeriodDetails.add(budgetPeriod);
			}
		}
		preapareExtendedPeriodByCalendarYear(budgetPeriods, calendar, budget);
		budgetPeriods.addAll(budgetPeriodDetails);
		return budgetPeriods;
	}

	private Date getEndDateForPeriod(Calendar cl, Date projectEndDate, Date periodStartDate) {
		Date periodEndDate;
		int lastDateOfMonth = cl.get(Calendar.DATE);
		int lastMonthOfYear = cl.get(Calendar.MONTH);
		if (lastDateOfMonth == 31 && lastMonthOfYear == 11) {
			cl.add(Calendar.YEAR, 1);
			int currentPeriodYear = cl.get(Calendar.YEAR);
			periodEndDate = new Date(cl.getTime().getTime());
			if (currentPeriodYear < getYear(projectEndDate)) {
				periodEndDate = getEndDateForaYear(currentPeriodYear);
			} else {
				periodEndDate = projectEndDate;
			}
		} else {
			cl.setTime(periodStartDate);
			cl.add(Calendar.DATE, 1);
			periodEndDate = new Date(cl.getTime().getTime());
			int currentPeriodYear = cl.get(Calendar.YEAR);

			if (currentPeriodYear < getYear(projectEndDate)) {
				periodEndDate = getEndDateForaYear(currentPeriodYear);
			} else {
				periodEndDate = projectEndDate;
			}
		}
		return periodEndDate;
	}

	private List<BudgetPeriod> preapareExtendedPeriodByCalendarYear(List<BudgetPeriod> budgetPeriods, Calendar calendar,
			BudgetHeader budget) {
		if (!budgetPeriods.isEmpty()) {
			calendar.setTime(budgetPeriods.get(budgetPeriods.size() - 1).getStartDate());
			calendar.add(Calendar.YEAR, 1);
			Date previousPeriodEndDate = new Date(calendar.getTime().getTime());
			calendar.setTime(previousPeriodEndDate);
			Integer previousYear = calendar.get(Calendar.YEAR);
			calendar.setTime(new Date(budget.getEndDate().getTime()));
			Integer newBudgetYear = calendar.get(Calendar.YEAR);
			if (previousYear <= newBudgetYear && (new Date(budget.getEndDate().getTime())
					.compareTo(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate()) != 0)) {
				prepareBudgetPeriodsByCalendarYear(budgetPeriods, previousPeriodEndDate, budget);
			}
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareBudgetDatesBasedOnCalenderYear(List<BudgetPeriod> budgetPeriods,
			BudgetHeader budget, Date projectStartDate, Date projectEndDate, int newStartYear) {
		Calendar cl = Calendar.getInstance();
		if (budget.getBudgetPeriods() != null && !budget.getBudgetPeriods().isEmpty()) {
			budgetPeriods = prepareBudgetPeriodsBasedOnCalenderYear(budgetPeriods, cl, budget, projectStartDate,
					projectEndDate, newStartYear);
		}
		return budgetPeriods.stream().sorted(Comparator.comparing(BudgetPeriod::getBudgetPeriod))
				.collect(Collectors.toList());
	}

	private List<BudgetPeriod> prepareBudgetDates(List<BudgetPeriod> budgetPeriods, BudgetHeader budget,
			Date projectStartDate, Date projectEndDate, int newStartYear) {
		Calendar cl = Calendar.getInstance();
		if (budget.getBudgetPeriods() != null && !budget.getBudgetPeriods().isEmpty()) {
			setPeriodsForDiffYear(projectEndDate, newStartYear, budgetPeriods, cl, budget, projectStartDate);
		}
		return budgetPeriods.stream().sorted(Comparator.comparing(BudgetPeriod::getBudgetPeriod))
				.collect(Collectors.toList());
	}

	private List<BudgetPeriod> setPeriodsForDiffYear(Date projectEndDate, int newStartYear,
			List<BudgetPeriod> budgetPeriods, Calendar cl, BudgetHeader budget, Date projectStartDate) {
		List<BudgetPeriod> budgetPeriodDetails = new ArrayList<>();
		Calendar calendar = Calendar.getInstance();
		List<BudgetPeriod> periods = budget.getBudgetPeriods().stream()
				.sorted(Comparator.comparing(BudgetPeriod::getBudgetPeriod)).collect(Collectors.toList());
		Calendar cal = Calendar.getInstance();
		cal.setTime(new Date(budget.getStartDate().getTime()));
		Integer count = 0;
		for (BudgetPeriod budgetPeriod : periods) {
			if (count < 2) {
				Date periodStartDate = projectStartDate;
				Date nextPeriodDate = periodStartDate;
				cl.setTime(periodStartDate);
				cl.add(Calendar.YEAR, 1);
				cl.add(Calendar.DATE, -1);
				Date periodEndDate = new Date(cl.getTime().getTime());
				if (periodEndDate.compareTo(new Date(budget.getEndDate().getTime())) >= 0) {
					periodEndDate = budget.getEndDate();
					count++;
				}
				if (periodStartDate.compareTo(new Date(budget.getStartDate().getTime())) <= 0) {
					periodStartDate = budget.getStartDate();
				}
				if (periodEndDate.compareTo(projectEndDate) >= 0) {
					if (count == 1) {
						Timestamp periodEndDateTimeStamp = new Timestamp(projectEndDate.getTime());
						budgetPeriod.setEndDate(periodEndDateTimeStamp);
						Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
						budgetPeriod.setStartDate(periodStartDateTimeStamp);
						budgetPeriod.setBudget(budget);
						budgetPeriods.add(budgetPeriod);
						count++;
					}
				} else {
					prepareSingleBudgetBeriod(periodStartDate, periodEndDate, budgetPeriods, budgetPeriod, budget);
				}
				if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
					newStartYear = getYear(
							new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime()));
				}
				newStartYear++;
				if (!budgetPeriods.isEmpty()) {
					nextPeriodDate = new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime());
					cl.setTime(nextPeriodDate);
					cl.add(Calendar.DATE, 1);
					projectStartDate = new Date(cl.getTime().getTime());
				}
			} else {
				budgetPeriod.setBudget(budget);
				budgetPeriodDetails.add(budgetPeriod);
			}
		}
		prepareExtendedPeriod(budgetPeriods, calendar, budget);
		budgetPeriods.addAll(budgetPeriodDetails);
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareExtendedPeriod(List<BudgetPeriod> budgetPeriods, Calendar calendar,
			BudgetHeader budget) {
		if (!budgetPeriods.isEmpty()) {
			Date newPeriodEndDate = new Date(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate().getTime());
			calendar.setTime(budgetPeriods.get(budgetPeriods.size() - 1).getStartDate());
			calendar.add(Calendar.YEAR, 1);
			Date previousPeriodEndDate = new Date(calendar.getTime().getTime());
			if (previousPeriodEndDate.compareTo(new Date(budget.getEndDate().getTime())) < 0
					&& previousPeriodEndDate.compareTo(new Date(budget.getEndDate().getTime())) != 0
					&& newPeriodEndDate.compareTo(new Date(budget.getEndDate().getTime())) != 0
					&& (new Date(budget.getEndDate().getTime())
							.compareTo(budgetPeriods.get(budgetPeriods.size() - 1).getEndDate()) != 0)) {
				prepareBudgetPeriods(budgetPeriods, previousPeriodEndDate, budget);
			} else {
				budgetPeriods.get(budgetPeriods.size() - 1).setEndDate(budget.getEndDate());
			}
		}
		return budgetPeriods;
	}

	private List<BudgetPeriod> prepareSingleBudgetBeriod(Date periodStartDate, Date periodEndDate,
			List<BudgetPeriod> budgetPeriods, BudgetPeriod budgetPeriod, BudgetHeader budget) {
		Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
		budgetPeriod.setEndDate(periodEndDateTimeStamp);
		Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
		budgetPeriod.setStartDate(periodStartDateTimeStamp);
		budgetPeriod.setBudget(budget);
		budgetPeriods.add(budgetPeriod);
		return budgetPeriods;
	}

}

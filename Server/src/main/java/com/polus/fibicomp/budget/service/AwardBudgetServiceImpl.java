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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.polus.fibicomp.roles.pojo.PersonRoles;
import org.apache.commons.lang.StringUtils;
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
import com.polus.fibicomp.award.comparator.AwardBudgetDetailComparatorBySortOrder;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.datesandamounts.service.DatesAndAmountService;
import com.polus.fibicomp.award.expense.dao.AwardExpenseDao;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetail;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardBudgetImportInfo;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.award.vo.BudgetImportPeriodsInfo;
import com.polus.fibicomp.budget.common.pojo.ValidCeRateType;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetFundType;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetNonPersonDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.pojo.BudgetTemplate;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.vo.AwardBudgetHeaderDetail;
import com.polus.fibicomp.budget.vo.AwardBudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.AwardBudgetSummary;
import com.polus.fibicomp.budget.vo.AwardBudgetSummaryVO;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.budget.vo.AwardDetailBudgetVO;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.proposal.comparator.AwardBudgetDetailComparatorBySystemGenerated;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.wbs.dao.WBSDao;

@Transactional
@Service(value = "awardBudgetService")
public class AwardBudgetServiceImpl implements AwardBudgetService {

	protected static Logger logger = LogManager.getLogger(AwardBudgetServiceImpl.class.getName());

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	@Qualifier(value = "awardDao")
	private AwardDao awardDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	@Qualifier("awardBudgetCalculationService")
	private AwardBudgetCalculationService budgetCalculationService;

	@Autowired
	@Qualifier("dateTimeService")
	private DateTimeService dateTimeService;

	@Autowired
	@Qualifier(value = "awardService")
	private AwardService awardService;

	@Autowired
	private BudgetDao proposalBudgetDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private AwardExpenseDao awardExpenseDao;

	@Autowired
	private AwardBudgetCopyService awardBudgetCopyService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;
	
	@Autowired
	private WBSDao wbsDao;

	@Autowired
	public CommitteeDao committeeDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private ManpowerDao manpowerDao;

	@Autowired
	private ManpowerService manpowerService;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Autowired
	private DatesAndAmountService datesAndAmountService;

	private static final String LINE_ITEM_COST_CHANGE = "lineItemCostChange";
    private static final String  BUDGET_CATEGORY_CODE = "budgetCategoryCode";

	@Override
	public String createAwardBudget(AwardBudgetVO vo) {
		Integer awardId = vo.getAwardId();
		logger.info("awardId : {}",  awardId);
		logger.info("activityTypeCode : {}", vo.getActivityTypeCode());
		Award award = awardDao.getAwardDetailsById(awardId);
		if (!award.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
			List <String> currentEditableSections = sectionWiseEditDao.getSectionTypeCodeBasedOnTypeCode(award.getAwardVariationTypeCode());
			if (!currentEditableSections.contains(Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE)) {
				List<String> editableSections = sectionWiseEditDao.getAwardEditingSectionsBasedOnParams(award.getAwardNumber());
				if (editableSections.contains(Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE)) {
					vo.setMessage("There is already an In Progress Budget Variation.Please complete Budget Variation before modifying Budget.");
					return commonDao.convertObjectToJSON(vo);
				} else {
					addToModuleVariableSection(Constants.BUDGET_EDITABLE_SECTION_TYPE_CODE, award.getAwardId(), award.getAwardVariationTypeCode());
				}
			}
		}
		AwardBudgetHeader awardBudgetHeader = null;
		AwardBudgetHeader copyAwardBudgetHeader = null;
		String defaultAbPersonItemToBeTBN = commonDao.getParameterValueAsString(Constants.DEFAULT_AB_TBN_PERSON_ID);
		if (vo.getAcType().equals("CREATE_NEW_BUDGET")) {
			awardBudgetHeader = awardBudgetDao.saveBudgetHeader(createAwardBudgetHeader(vo, award));
			if (defaultAbPersonItemToBeTBN != null && !defaultAbPersonItemToBeTBN.isEmpty()) {
				createDefaultAwardBudgetPersonItem(awardBudgetHeader.getBudgetId(), awardBudgetHeader.getUpdateUser(), defaultAbPersonItemToBeTBN);
			}
			for (AwardBudgetPeriod budgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
				budgetPeriod.setVersionNumber(1);
				budgetPeriod.setBudgetId(awardBudgetHeader.getBudgetId());
				budgetPeriod.setAwardNumber(awardBudgetHeader.getAwardNumber());
				budgetPeriod.setUpdateUser(awardBudgetHeader.getUpdateUser());
				awardBudgetHeader.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardBudgetDao.saveBudgetPeriod(budgetPeriod);
			}
		} else {
			AwardVO awardVO = new AwardVO();
			awardVO.setUpdateUser(vo.getUserName());
			awardVO.setUserFullName(vo.getUserFullName());
			copyAwardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardId);
			fetchAwardBudgetPeriods(copyAwardBudgetHeader);
			awardVO.setServiceRequestTypeCode(Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE);
			awardBudgetHeader = awardBudgetCopyService.createAwardBudgetHeader(awardVO, award, copyAwardBudgetHeader, Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE, null);
			awardBudgetHeader.setIsAutoCalc(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AWARD_BUDGET_AUTO_CALCULATE) ? commonDao.getParameterValueAsBoolean(Constants.DEFAULT_AWARD_BUDGET_IS_AUTO_CALCULATION) : Boolean.FALSE);
			if (Boolean.TRUE.equals(awardBudgetHeader.getIsAutoCalc())) {
				Set<String> rateClassTypes = new HashSet<>();
				List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(awardBudgetHeader, award.getActivityTypeCode(),
						rateClassTypes);
				saveOrUpdateAwardBudgetRates(awardBudgetRates);
				awardBudgetHeader.getAwardRates().addAll(awardBudgetRates);
				vo.setAwardRates(awardBudgetRates);
				setRateClassType(awardBudgetRates, vo);
			}
		}
		if (commonDao.getParameterValueAsBoolean(Constants.COST_ELEMENT_FROM_TEMPLATE)) {
			vo.setBudgetTemplateTypes(budgetDao.getBudgetTemplateTypesByModuleCode(Constants.AWARD_MODULE_CODE));
		}
		awardBudgetHeader.setFundCode(award.getAccountNumber());
		awardBudgetHeader.setFundCenter(award.getFundCenter());
		awardBudgetHeader.setInitialAvailableFund(calculateAvailableFund(vo.getAwardId(), awardBudgetHeader.getAwardNumber(), true, vo.getAvailableFundType()));
		awardBudgetHeader.setAvailableFund(awardBudgetHeader.getInitialAvailableFund().subtract(awardBudgetHeader.getTotalCost()));
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		awardBudgetHeader.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		awardBudgetHeader.setManpowerEnabled(manpowerEnabled);
		awardBudgetHeader.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
		vo.setSysGeneratedCostElements(fetchSysGeneratedCostElements(vo.getActivityTypeCode()));
		vo.setIsCreated(true);
		List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(vo.getAwardId());
		if (awardBudgetHeaders != null && !awardBudgetHeaders.isEmpty()) {
			List<AwardBudgetHeaderDetail> awardBudgetHeaderDetails = new ArrayList<>();
			for (AwardBudgetHeader awardBudget : awardBudgetHeaders) {
				awardBudgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(awardBudget));
			}
			vo.setAwardBudgetList(awardBudgetHeaderDetails);
		}
		vo.setAwardBudgetHeader(awardBudgetHeader);
		vo.setDefaultAbPersonItemToBeTBN(defaultAbPersonItemToBeTBN);
		String ohRateClassTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE);
		vo.setRateTypes(proposalBudgetDao.fetchRateTypeByParams("1", proposalBudgetDao.fetchRateClassCodesByType(ohRateClassTypeCode)));
		if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AWARD_BUDGET_AUTO_CALCULATE)) {
			vo.setIsAutoCalculationEnabled(true);
		}
		if (commonDao.getParameterValueAsBoolean(Constants.BUDGET_ASSOCIATED_WITH_NON_PERSONAL_SUBITEM_ENABLED)) {
			vo.setIsNonPersonalLineItemEnabled(true);
		}
		vo.setModifyABInRouting(commonDao.getParameterValueAsBoolean(Constants.MODIFY_AB_IN_ROUTING));
		vo.setEnableAbPersonAppliedSalary(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_PERSON_APPL_SAL_CALC));
		vo.setEnabledCampusFlagAward(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_AWARD));
		vo.setShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
		boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
		vo.setEnableCostShareStatus(isEnableCostShareStatus);
		if(isEnableCostShareStatus) {
			vo.setCostSharingTypes(budgetDao.getCostSharingType());
		}
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardId, vo.getUserName());
		boolean isShowAwardBudgetFieldForSap = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_FIELDS_FOR_SAP);
		vo.setShowAwardBudgetFieldForSap(isShowAwardBudgetFieldForSap);
		if (isShowAwardBudgetFieldForSap) {
			vo.setFundDisbursementBasisTypes(budgetDao.getFundDisbursementBasisType());
		}
		return committeeDao.convertObjectToJSON(vo);
	}

	private void addToModuleVariableSection(String sectionCode, Integer awardId, String awardVariationTypeCode) {
		ModuleVariableSection moduleVariableSection = new ModuleVariableSection();
		moduleVariableSection.setModuleCode(Constants.AWARD_MODULE_CODE);
		moduleVariableSection.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE);
		moduleVariableSection.setModuleItemKey(awardId.toString());
		moduleVariableSection.setSectionCode(sectionCode);
		moduleVariableSection.setSectionType(sectionWiseEditDao.getSectionTypebySectionTypeId(sectionCode));
		moduleVariableSection.setTypeCode(Integer.parseInt(awardVariationTypeCode));
		moduleVariableSection.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		moduleVariableSection.setPersonId(AuthenticatedUser.getLoginPersonId());
		moduleVariableSection.setSubModuleItemKey(Constants.ZERO);
		moduleVariableSection.setUpdateUser(AuthenticatedUser.getLoginUserName());
		moduleVariableSection.setVariableType(Constants.VARIATION_REQUEST_VARIABLE_TYPE);
		sectionWiseEditDao.saveorUpdateModuleVariableSection(moduleVariableSection);
	}

	private void addTemplateCostElementForAward(AwardBudgetHeader awardBudgetHeader, String defaultAbPersonItemToBeTBN, Award award, Integer budgetTemplateTypeId) {
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		List<AwardBudgetDetail> newAwardBudgetDetails = new ArrayList<>();
		AwardBudgetPeriod awardBudgetPeriod = awardBudgetHeader.getBudgetPeriods().get(0);
		List<BudgetTemplate> budgetTemplates = budgetDao.fetchBudgetTemplatesByTemplateType(budgetTemplateTypeId);
		Integer lineItemNumber = 1;
		for (BudgetTemplate budgetTemplate : budgetTemplates) {
			AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
			CostElement costElement = awardBudgetDao.fetchCostElementsById(budgetTemplate.getCostElement());
				if (costElement.isActive()) {
					awardBudgetDetail.setSystemGeneratedCEType(budgetTemplate.getSystemGeneratedCEType());
					awardBudgetDetail.setBudgetId(awardBudgetHeader.getBudgetId());
					awardBudgetDetail.setAwardNumber(awardBudgetHeader.getAwardNumber());
					awardBudgetDetail.setBudgetPeriodId(awardBudgetPeriod.getBudgetPeriodId());
					awardBudgetDetail.setVersionNumber(awardBudgetPeriod.getVersionNumber());
					awardBudgetDetail.setBudgetPeriod(awardBudgetPeriod.getBudgetPeriod());
					awardBudgetDetail.setLineItemNumber(lineItemNumber);
					awardBudgetDetail.setBudgetCategoryCode(costElement.getBudgetCategoryCode());
					awardBudgetDetail.setBudgetCategory(budgetDao.fetchBudgetCategoryBasedOnCode(costElement.getBudgetCategoryCode()));
					awardBudgetDetail.setCostElementCode(costElement.getCostElement());
					awardBudgetDetail.setCostElement(costElement);
					awardBudgetDetail.setLineItemCost(BigDecimal.ZERO);
					awardBudgetDetail.setPrevLineItemCost(null);
					awardBudgetDetail.setIsSystemGeneratedCostElement(budgetTemplate.getIsSystemGeneratedCostElement());
					awardBudgetDetail.setTbnId(defaultAbPersonItemToBeTBN);
					String lineItemDescription = "Default Item " + lineItemNumber;
					awardBudgetDetail.setLineItemDescription(lineItemDescription);
					awardBudgetDetail.setUpdateUser(awardBudgetHeader.getUpdateUser());
					awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
					newAwardBudgetDetails.add(awardBudgetDetail);
					lineItemNumber++;
				}
		}
		List<CostElement> sysGeneratedCostElements = fetchSysGeneratedCostElements(award.getActivityTypeCode());
		for (CostElement costElements : sysGeneratedCostElements) {
			newAwardBudgetDetails.add(setAwardBudgetDetailData(costElements, newAwardBudgetDetails.get(newAwardBudgetDetails.size() - 1), lineItemNumber));
			lineItemNumber++;
		}
		resetTheBudgetDetails(awardBudgetHeader.getBudgetPeriods());
		newAwardBudgetDetails.stream().forEach(awdBudgetDetail -> {
			awardBudgetDao.saveOrUpdateAwardBudgetLineItem(awdBudgetDetail);
			awardBudgetDetails.add(awdBudgetDetail);
		});
		awardBudgetPeriod.setBudgetDetails(awardBudgetDetails);
	}

	private void resetTheBudgetDetails(List<AwardBudgetPeriod> awardBudgetPeriods) {
		awardBudgetPeriods.stream().forEach(awardBudgetPeriod -> {
			awardBudgetDao.deleteAllBudgetDetail(awardBudgetPeriod.getBudgetDetails());
			awardBudgetPeriod.getBudgetDetails().clear();
		});
	}

	private void createDefaultAwardBudgetPersonItem(Integer budgetId, String updateUser, String defaultAbPersonItemToBeTBN) {
		AwardBudgetPerson awardBudgetPerson = new AwardBudgetPerson();
		awardBudgetPerson.setBudgetHeaderId(budgetId);
		awardBudgetPerson.setPersonType(Constants.TBN_PERSON_TYPE);
		awardBudgetPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardBudgetPerson.setUpdateUser(updateUser);
		awardBudgetPerson.setTbnId(defaultAbPersonItemToBeTBN);
		awardBudgetDao.saveOrUpdateAwardBudgetPerson(awardBudgetPerson);
	}

	@Override
	public AwardBudgetHeader createAwardBudgetHeader(AwardBudgetVO vo, Award award) {
		AwardBudgetHeader budgetHeader = new AwardBudgetHeader();
		budgetHeader.setStartDate(award.getBeginDate());
		budgetHeader.setEndDate(award.getFinalExpirationDate());
		budgetHeader.setCreateTimeStamp(commonDao.getCurrentTimestamp());
		budgetHeader.setCreateUser(vo.getUserName());
		budgetHeader.setCreateUserName(personDao.getPersonIdByUserName(vo.getUserName()));
		budgetHeader.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budgetHeader.setUpdateUser(vo.getUserName());
		budgetHeader.setUpdateUserName(vo.getUserFullName());
		budgetHeader.setBudgetStatusCode(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS);
		budgetHeader.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS));
		if (vo.getAcType().equals("CREATE_NEW_BUDGET")) {
			budgetHeader.setBudgetTypeCode(Constants.BUDGET_TYPE_NEW);
			budgetHeader.setBudgetType(awardBudgetDao.getBudgetTypeById(Constants.BUDGET_TYPE_NEW));
		} else {
			budgetHeader.setBudgetTypeCode(Constants.BUDGET_TYPE_REBUDGET);
			budgetHeader.setBudgetType(awardBudgetDao.getBudgetTypeById(Constants.BUDGET_TYPE_REBUDGET));
		}
		budgetHeader.setAwardId(vo.getAwardId());
		budgetHeader.setAwardNumber(vo.getAwardNumber());
		String rateClassCode = commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_CLASS_CODE);
		String rateTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_TYPE_CODE);
		logger.info("rateClassCode : {}", rateClassCode);
		logger.info("rateTypeCode : {}", rateTypeCode);
		budgetHeader.setRateClassCode(rateClassCode);
		budgetHeader.setRateTypeCode(rateTypeCode);
		budgetHeader.setRateType(awardBudgetDao.getOHRateTypeByParams(rateClassCode, rateTypeCode));
		budgetHeader.setOnOffCampusFlag("N");
		Integer versionNumber = awardBudgetDao.maxAwardBudgetVersionNumberByAwardId(vo.getAwardId());
		if (versionNumber != null) {
			budgetHeader.setVersionNumber(versionNumber + 1);
		} else {
			budgetHeader.setVersionNumber(1);
		}
		budgetHeader.setSequenceNumber(award.getSequenceNumber());
		budgetHeader.setTotalCost(BigDecimal.ZERO);
		budgetHeader.setTotalDirectCost(BigDecimal.ZERO);
		budgetHeader.setTotalIndirectCost(BigDecimal.ZERO);
		budgetHeader.setIsLatestVersion(true);
		budgetHeader.setFundCode(award.getAccountNumber());
		budgetHeader.setFundCenter(award.getFundCenter());
		budgetHeader.setIsAutoCalc(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AWARD_BUDGET_AUTO_CALCULATE) ? commonDao.getParameterValueAsBoolean(Constants.DEFAULT_AWARD_BUDGET_IS_AUTO_CALCULATION) : Boolean.FALSE);
		budgetHeader.setAvailableFundType(vo.getAvailableFundType());
		budgetHeader.setInitialAvailableFund(calculateAvailableFund(vo.getAwardId(), award.getAwardNumber(), true, vo.getAvailableFundType()));
		budgetHeader.setAvailableFund(budgetHeader.getInitialAvailableFund().subtract(budgetHeader.getTotalCost()));
		budgetHeader.setAnticipatedTotal(calculateAnticipatedAmount(vo.getAwardId(), award.getAwardNumber()));
		budgetHeader.setObligatedTotal(calculateObligatedTotal(vo.getAwardId(), award.getAwardNumber()));
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		budgetHeader.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		budgetHeader.setManpowerEnabled(manpowerEnabled);
		budgetHeader.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
		budgetHeader.setTotalCostShare(calculateCostShareIncludedInBudget(vo.getAwardId()));
		awardBudgetDao.saveBudgetHeader(budgetHeader);
		if (Boolean.TRUE.equals(budgetHeader.getIsAutoCalc())) {
			Set<String> rateClassTypes = new HashSet<>();
			List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(budgetHeader, award.getActivityTypeCode(),
					rateClassTypes);
			saveOrUpdateAwardBudgetRates(awardBudgetRates);
			budgetHeader.getAwardRates().addAll(awardBudgetRates);
			vo.setAwardRates(awardBudgetRates);
			setRateClassType(awardBudgetRates, vo);
		}
		if (isSinglePeriodEnabled()) {
			budgetHeader.setBudgetPeriods(generateSingleBudgetPeriod(budgetHeader));
		} else {
			if (isPeriodGenerationBasedOnCalenderYear()) {
				budgetHeader.setBudgetPeriods(generateMultipleBudgetPeriodsByCalendarYear(budgetHeader));
			} else {
				budgetHeader.setBudgetPeriods(generateMultipleBudgetPeriods(budgetHeader));
			}
		}
		return budgetHeader;
	}

	@Override
	public String saveOrUpdateAwardBudget(AwardBudgetVO vo) {
		AwardBudgetHeader awardBudgetHeader = vo.getAwardBudgetHeader();
		awardBudgetDao.saveBudgetHeader(awardBudgetHeader);
		Integer awardId = vo.getAwardBudgetHeader().getAwardId();
		Award award = awardDao.getAwardDetailsById(awardId);
		award.setAccountNumber(awardBudgetHeader.getFundCode());
		award.setFundCenter(awardBudgetHeader.getFundCenter());
		award = awardDao.saveOrUpdateAwardDetails(award);
		vo.setAward(award);
		vo.setIsCreated(true);
		if (awardBudgetHeader.getVersionNumber() > 1) {
			setAwardBudgetHeaderBasedOnVersionNumber(awardBudgetHeader, vo);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public Award saveOrUpdateAwardBudget(AwardVO vo) {
		Award award = vo.getAward();
		awardDao.saveOrUpdateAwardDetails(award);
		return award;
	}

	@Override
	public List<AwardBudgetPeriod> generateBudgetPeriods(AwardBudgetHeader budget) {
		List<AwardBudgetPeriod> budgetPeriods = new ArrayList<>();
		Date projectStartDate = new Date(budget.getStartDate().getTime());
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;
		Calendar cl = Calendar.getInstance();
		Date periodStartDate = projectStartDate;
		int budgetPeriodNum = 1;
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
			AwardBudgetPeriod budgetPeriod = new AwardBudgetPeriod();
			budgetPeriod.setBudgetPeriod(budgetPeriodNum);
			budgetPeriod.setStartDate(new Timestamp(periodStartDate.getTime()));
			budgetPeriod.setEndDate(new Timestamp(periodEndDate.getTime()));
			budgetPeriod.setBudgetId(budget.getBudgetId());

			budgetPeriods.add(budgetPeriod);
			periodStartDate = nextPeriodStartDate;
			budgetPeriodNum++;
		}
		return budgetPeriods;
	}

	@Override
	public boolean budgetLineItemExists(AwardBudgetHeader budget, Integer budgetPeriod) {
		boolean lineItemExists = false;
		List<AwardBudgetDetail> budgetLineItems = budget.getBudgetPeriods().get(budgetPeriod).getBudgetDetails();
		for (AwardBudgetDetail periodLineItem : budgetLineItems) {
			Integer lineItemPeriod = periodLineItem.getBudgetPeriod();
			if (budgetPeriod + 1 == lineItemPeriod) {
				lineItemExists = true;
				break;
			}
		}
		return lineItemExists;
	}

	protected int getYear(Date date) {
		Calendar c1 = Calendar.getInstance();
		c1.setTime(new java.util.Date(date.getTime()));
		return c1.get(Calendar.YEAR);
	}

	@Override
	public List<CostElement> fetchSysGeneratedCostElements(String activityTypeCode) {
		List<CostElement> systemGeneratedCE = new ArrayList<>();
		if (activityTypeCode != null && "1".equals(activityTypeCode)) {
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
	public String deletePersonnelLine(AwardBudgetVO awardVO) {
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_PERSON_APPL_SAL_CALC)) {
			updateLineItemCost(awardVO);
		}
		awardBudgetDao.deleteBudgetPersonDetail(awardVO.getBudgetPersonDetailId());
		awardVO.setStatus(true);
		awardVO.setMessage("Budget person deleted successfully");
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardVO.getAwardId(), awardVO.getUserName());
		return commonDao.convertObjectToJSON(calculateAwardBudget(awardVO.getBudgetHeaderId()));
	}

	private void updateLineItemCost(AwardBudgetVO awardVO) {
		BigDecimal deletedPersonalLineItemCost = BigDecimal.ZERO;
		BigDecimal lineItemCostAfterDelete = BigDecimal.ZERO;
		AwardBudgetPersonalDetail awardBudgetPersonalDetail = awardBudgetDao.getAwardBudgetPersonalDetailsByPersonDetailId(awardVO.getBudgetPersonDetailId());
		if (awardBudgetPersonalDetail != null && awardBudgetPersonalDetail.getSalaryRequested() != null) {
			deletedPersonalLineItemCost = awardBudgetPersonalDetail.getSalaryRequested();
			AwardBudgetDetail awardBudgetDetail = awardBudgetDao.getAwardBudgetDetailsByDetailId(awardBudgetPersonalDetail.getBudgetDetail().getBudgetDetailId());
			if (awardBudgetDetail != null) {
				lineItemCostAfterDelete = (awardBudgetDetail.getLineItemCost().subtract(deletedPersonalLineItemCost));
				awardBudgetDetail.setLineItemCost(lineItemCostAfterDelete);
				awardBudgetDao.saveOrUpdateAwardBudgetLineItem(awardBudgetDetail);
			}
		}
	}

	private Date getEndDateForaYear(int year) {
		Date date = new Date();
		SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
		String dateInString = "31/12/".concat(String.valueOf(year));
		try {
			date = formatter.parse(dateInString);
		} catch (Exception e) {
			logger.error("Error in reset get End Date For a Year {}", e.getMessage());
		}
		return date;
	}

	private boolean isPeriodGenerationBasedOnCalenderYear() {
		return commonDao.getParameterValueAsBoolean(Constants.AWARD_BUDGET_GENERATION_BASED_ON_CALENDAR_YEAR);
	}

	@Override
	public String loadBudgetByAwardId(AwardBudgetVO vo) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		awardBudgetHeader.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		fetchAwardBudgetPeriods(awardBudgetHeader);
		if (awardBudgetHeader.getUpdateUser() != null) {
			awardBudgetHeader.setUpdateUserName(personDao.getUserFullNameByUserName(awardBudgetHeader.getUpdateUser()));
		}
		setBudgetDetails(awardBudgetHeader);
		if (awardBudgetHeader.getBudgetStatusCode().equals(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS)) {
			updateBudgetHeader(awardBudgetHeader);
		}
		vo.setAwardBudgetHeader(awardBudgetHeader);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT)) {
			vo.setSysGeneratedCostElements(fetchSysGeneratedCostElements(vo.getActivityTypeCode()));
		}
		if (awardBudgetHeader.getVersionNumber() > 1) {
			setAwardBudgetHeaderBasedOnVersionNumber(awardBudgetHeader, vo);
		}
		setAwardBudgetRates(vo, awardBudgetHeader);
		vo.setIsCreated(true);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AWARD_BUDGET_AUTO_CALCULATE)) {
		vo.setIsAutoCalculationEnabled(true);
		}
		vo.setCostSharingTypeCode(awardBudgetHeader.getCostSharingTypeCode());
		boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
		vo.setEnableCostShareStatus(isEnableCostShareStatus);
		if (isEnableCostShareStatus) {
			vo.setCostSharingTypes(budgetDao.getCostSharingType());
		}
		if (commonDao.getParameterValueAsBoolean(Constants.BUDGET_ASSOCIATED_WITH_NON_PERSONAL_SUBITEM_ENABLED)) {
			vo.setIsNonPersonalLineItemEnabled(true);
		}
		if (commonDao.getParameterValueAsBoolean(Constants.COST_ELEMENT_FROM_TEMPLATE)) {
			vo.setBudgetTemplateTypes(budgetDao.getBudgetTemplateTypesByModuleCode(Constants.AWARD_MODULE_CODE));
		}
		vo.setModifyABInRouting(commonDao.getParameterValueAsBoolean(Constants.MODIFY_AB_IN_ROUTING));
		vo.setEnableAbPersonAppliedSalary(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_PERSON_APPL_SAL_CALC));
		vo.setDefaultAbPersonItemToBeTBN(commonDao.getParameterValueAsString(Constants.DEFAULT_AB_TBN_PERSON_ID));
		String ohRateClassTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE);
		vo.setRateTypes(proposalBudgetDao.fetchRateTypeByParams("1", proposalBudgetDao.fetchRateClassCodesByType(ohRateClassTypeCode)));
		vo.setIsSinglePeriodBudgetEnabled(commonDao.getParameterValueAsBoolean(Constants.AWARD_BUDGET_SINGLE_PERIOD_ENABLED));
		vo.setEnabledCampusFlagAward(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_AWARD));
		vo.setShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
		boolean isShowAwardBudgetFieldForSap = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_FIELDS_FOR_SAP);
		vo.setShowAwardBudgetFieldForSap(isShowAwardBudgetFieldForSap);
		if (isShowAwardBudgetFieldForSap) {
			vo.setFundDisbursementBasisTypes(budgetDao.getFundDisbursementBasisType());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void setAwardBudgetRates(AwardBudgetVO vo, AwardBudgetHeader awardBudgetHeader) {
		List<AwardRates> awardRates = getAwardRatesByBudgetId(awardBudgetHeader.getBudgetId());
		awardBudgetHeader.getAwardRates().clear();
		awardBudgetHeader.getAwardRates().addAll(awardRates);
		vo.setAwardRates(awardRates);
		setRateClassType(awardRates, vo);
		vo.setIsAutoCalculationEnabled(awardBudgetHeader.getIsAutoCalc());
	}
	
	@Override
	public List<AwardBudgetPeriod> fetchAwardBudgetPeriods(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetPeriods.addAll(awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId()));
		Set<Integer> budgetPeriodIds = awardBudgetPeriods.stream().map(AwardBudgetPeriod :: getBudgetPeriodId).collect(Collectors.toSet());
		List<AwardBudgetDetail> allBudgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodIds(budgetPeriodIds);
		Boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		Set<Integer> budgetDetailIds = allBudgetDetails.stream().map(AwardBudgetDetail :: getBudgetDetailId).collect(Collectors.toSet());
		List<AwardBudgetDetailCalcAmount> budgetDetailCalAmounts = awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailIds(budgetDetailIds);
		Map<Integer, List<AwardBudgetDetailCalcAmount>> collect = budgetDetailCalAmounts.stream().collect(Collectors.groupingBy(AwardBudgetDetailCalcAmount :: getBudgetDetailId));
		allBudgetDetails.stream().filter(item -> collect.containsKey(item.getBudgetDetailId())).forEach(item -> item.setBudgetDetailCalcAmounts(collect.get(item.getBudgetDetailId())));
		if(Boolean.TRUE.equals(manpowerEnabled)) {
																										  
	 
										   
	
			setManpowerData(allBudgetDetails, awardBudgetHeader);
		}
		Map<Integer, List<AwardBudgetDetail>> mapBudgetDetail = allBudgetDetails.stream().collect(Collectors.groupingBy(AwardBudgetDetail :: getBudgetPeriodId));
		awardBudgetPeriods.stream().filter(period -> mapBudgetDetail.containsKey(period.getBudgetPeriodId())).forEach(period -> period.setBudgetDetails(mapBudgetDetail.get(period.getBudgetPeriodId())));
		awardBudgetHeader.getBudgetPeriods().addAll(awardBudgetPeriods);
		return awardBudgetHeader.getBudgetPeriods();
	}

	private void setManpowerData(List<AwardBudgetDetail> allBudgetDetails, AwardBudgetHeader awardBudgetHeader) {
		Set<String> budgetReferenceNumbers = allBudgetDetails.stream().map(awardBudgetDetail -> awardBudgetDetail.getInternalOrderCode() != null ?
				awardBudgetDetail.getInternalOrderCode() : awardBudgetDetail.getBudgetDetailId().toString()).collect(Collectors.toSet());
		List<AwardManpower> awardManpower = manpowerDao.fetchAwardManpowerDetailsByIOCode(budgetReferenceNumbers, awardBudgetHeader.getAwardId());
		Set<Integer> awardManpowerIds = awardManpower.stream().map(AwardManpower :: getAwardManpowerId).collect(Collectors.toSet());
		List<AwardManpowerResource> awardManpowerResources = manpowerDao.getAllAwardManpowerResourcesByManpowerIds(awardManpowerIds);
		Map<String, AwardManpower> mapAwardManpower = awardManpower.stream().collect(Collectors.toMap(AwardManpower :: getBudgetReferenceNumber, manpower -> manpower));
		Map<Integer, List<AwardManpowerResource>> mapAwardManpowerResource = awardManpowerResources.stream().collect(Collectors.groupingBy(AwardManpowerResource :: getAwardManpowerId));
		allBudgetDetails.stream().filter(item -> mapAwardManpower.containsKey(item.getInternalOrderCode() != null ?
				item.getInternalOrderCode() : item.getBudgetDetailId().toString())).forEach(budgetDetail -> {
			AwardManpower awardMan = mapAwardManpower.get(budgetDetail.getInternalOrderCode() != null ? budgetDetail.getInternalOrderCode() : budgetDetail.getBudgetDetailId().toString());
			List<AwardManpowerResource> awardManpowerResources1 = new ArrayList<>();
			budgetDetail.setIsManpowerResourceExistInManpower(mapAwardManpowerResource.containsKey(awardMan.getAwardManpowerId()));
			if(budgetDetail.getIsManpowerResourceExistInManpower()) {
				awardManpowerResources1 = mapAwardManpowerResource.get(awardMan.getAwardManpowerId());
			}
			budgetDetail.setIsIOCodeExistInManpower(Boolean.TRUE);
			BigDecimal actualCommittedCost = BigDecimal.ZERO;
			BigDecimal plannedSalary;
			if(!awardMan.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_OTHER)) {
				actualCommittedCost = awardManpowerResources1.stream().filter(x -> x.getCommittedCost() != null)
						.map(AwardManpowerResource ::getCommittedCost ).reduce(BigDecimal.ZERO,BigDecimal::add);
			}
			plannedSalary = awardManpowerResources1.stream().filter(x -> x.getCommittedCost() == null && x.getPlannedSalary() != null)
					.map(AwardManpowerResource ::getPlannedSalary ).reduce(BigDecimal.ZERO,BigDecimal::add);
			budgetDetail.setAwardManpowerCommittedCost(actualCommittedCost.add(plannedSalary));
		});
	}

	private AwardBudgetDetail setManpowerCommittedCost(AwardBudgetDetail awardBudgetDetail, Integer awardId) {
		if (awardBudgetDetail.getBudgetCategoryCode() != null && (awardBudgetDetail.getBudgetCategoryCode().equals("EOM") ||
				awardBudgetDetail.getBudgetCategoryCode().equals("RSS"))) {
			String budgetReferenceNumber = awardBudgetDetail.getInternalOrderCode() != null ? awardBudgetDetail.getInternalOrderCode() : awardBudgetDetail.getBudgetDetailId().toString();
			AwardManpower awardManpower = manpowerDao.fetchAwardManpowerDetails(budgetReferenceNumber, awardId);
			if (awardManpower != null) {
				awardBudgetDetail.setIsManpowerResourceExistInManpower(manpowerDao.checkIfAwardManpowerResourceIsExistBasedOnParams(awardManpower.getAwardManpowerId()));
				awardBudgetDetail.setIsIOCodeExistInManpower(Boolean.TRUE);
				awardBudgetDetail.setAwardManpowerCommittedCost(manpowerService.calculateBudgetActualCommittedCost(awardManpower.getAwardManpowerId(), awardManpower.getManpowerTypeCode()));
			}
		}
		return awardBudgetDetail;
	}

	private boolean isSinglePeriodEnabled() {
		return commonDao.getParameterValueAsBoolean(Constants.AWARD_BUDGET_SINGLE_PERIOD_ENABLED);
	}

	private List<AwardBudgetPeriod> generateSingleBudgetPeriod(AwardBudgetHeader budgetHeader) {
		List<AwardBudgetPeriod> budgetPeriods = new ArrayList<>();
		AwardBudgetPeriod budgetPeriod = new AwardBudgetPeriod();
		budgetPeriod.setTotalCost(BigDecimal.ZERO);
		budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
		budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
		budgetPeriod.setBudgetPeriod(1);
		budgetPeriod.setStartDate(budgetHeader.getStartDate());
		budgetPeriod.setEndDate(budgetHeader.getEndDate());
		budgetPeriod.setBudgetId(budgetHeader.getBudgetId());
		budgetPeriod.setAwardNumber(budgetHeader.getAwardNumber());
		budgetPeriods.add(budgetPeriod);
		return budgetPeriods;
	}

	private List<AwardBudgetPeriod> generateMultipleBudgetPeriodsByCalendarYear(AwardBudgetHeader budgetHeader) {
		List<AwardBudgetPeriod> budgetPeriods = new ArrayList<>();
		Date projectStartDate = budgetHeader.getStartDate();
		Date projectEndDate = budgetHeader.getEndDate();
		boolean budgetPeriodExists = true;
		int budgetPeriodEndingYear = getYear(projectEndDate);
		Calendar cl = Calendar.getInstance();

		Date periodStartDate = projectStartDate;
		int budgetPeriodNum = 1;
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
			AwardBudgetPeriod budgetPeriod = new AwardBudgetPeriod();
			budgetPeriod.setBudgetPeriod(budgetPeriodNum);
			Timestamp periodStartDateTimeStamp = new Timestamp(periodStartDate.getTime());
			Timestamp periodEndDateTimeStamp = new Timestamp(periodEndDate.getTime());
			budgetPeriod.setStartDate(periodStartDateTimeStamp);
			budgetPeriod.setEndDate(periodEndDateTimeStamp);
			budgetPeriod.setBudgetId(budgetHeader.getBudgetId());
			budgetPeriod.setTotalCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
			budgetPeriods.add(budgetPeriod);
			cl.setTime(new Timestamp(nextPeriodStartDate.getTime()));
			cl.add(Calendar.DATE, 1);
			periodStartDate = new Date(cl.getTime().getTime());
			budgetPeriodNum++;
		}
		return budgetPeriods;
	}

	public List<AwardBudgetPeriod> generateMultipleBudgetPeriods(AwardBudgetHeader budget) {
		List<AwardBudgetPeriod> budgetPeriods = new ArrayList<AwardBudgetPeriod>();
		Date projectStartDate = new Date(budget.getStartDate().getTime());
		Date projectEndDate = new Date(budget.getEndDate().getTime());
		boolean budgetPeriodExists = true;

		Calendar cl = Calendar.getInstance();

		Date periodStartDate = projectStartDate;
		int budgetPeriodNum = 1;
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
			AwardBudgetPeriod budgetPeriod = new AwardBudgetPeriod();
			budgetPeriod.setTotalCost(BigDecimal.ZERO);
			budgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
			budgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
			budgetPeriod.setBudgetPeriod(budgetPeriodNum);
			budgetPeriod.setStartDate(new Timestamp(periodStartDate.getTime()));
			budgetPeriod.setEndDate(new Timestamp(periodEndDate.getTime()));
			budgetPeriod.setBudgetId(budget.getBudgetId());

			budgetPeriods.add(budgetPeriod);
			periodStartDate = nextPeriodStartDate;
			budgetPeriodNum++;
		}
		return budgetPeriods;
	}

	@Override
	public String getDevProposalBudgetByAwardId(AwardBudgetVO vo) {
		List<AwardBudgetImportInfo> awardBudgetImportInfos = new ArrayList<>();
		List<AwardBudgetImportInfo> newAwardBudgetImportInfos = new ArrayList<>();
		List<BudgetImportPeriodsInfo> budgetImportPeriods = new ArrayList<>();
		awardBudgetImportInfos = awardBudgetDao.getDevProposalBudgets(vo.getAwardId());
		for (AwardBudgetImportInfo awardBudgetImportInfo : awardBudgetImportInfos) {
			budgetImportPeriods = awardBudgetDao.getDevProposalBudgetPeriods(awardBudgetImportInfo.getPdBudgetId());
			awardBudgetImportInfo.setBudgetImportPeriods(budgetImportPeriods);
			newAwardBudgetImportInfos.add(awardBudgetImportInfo);
		}
		vo.setAwardBudgetImportInfos(newAwardBudgetImportInfos);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String importProposalBudget(AwardBudgetVO vo) {
		Boolean isSinglePeriodAward = commonDao.getParameterValueAsBoolean(Constants.AWARD_BUDGET_SINGLE_PERIOD_ENABLED);
		AwardBudgetHeader awdBudgetHeader = vo.getAwardBudgetHeader();
		awdBudgetHeader.getBudgetPeriods().clear();
		fetchAwardBudgetPeriods(awdBudgetHeader);
		saveBudgetPeriodData(awdBudgetHeader.getBudgetPeriods(), awdBudgetHeader.getBudgetId());
		BudgetHeader budgetHeader = proposalBudgetDao.fetchBudgetByBudgetId(vo.getDevPropBudgetHeaderId());
		if (Boolean.TRUE.equals(vo.getIsAllPeriod())) {
			List<AwardBudgetPeriod> newBudgetPeriods = new ArrayList<>();
			List<AwardBudgetPeriod> awdBudgetPeriods = awdBudgetHeader.getBudgetPeriods();
			if (Boolean.FALSE.equals(isSinglePeriodAward)) {
				for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
					for (AwardBudgetPeriod awdBudgetPeriod : awdBudgetPeriods) {
						if (budgetPeriod.getBudgetPeriod().equals(awdBudgetPeriod.getBudgetPeriod())) {
							if (awdBudgetPeriod.getBudgetDetails() != null
									&& !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
								awdBudgetPeriod = new AwardBudgetPeriod();
							}
							BigDecimal periodTotalCost = BigDecimal.ZERO;
							BigDecimal periodTotalDirectCost = BigDecimal.ZERO;
							BigDecimal periodTotalIndirectCost = BigDecimal.ZERO;
							awdBudgetPeriod.setStartDate(awdBudgetPeriod.getStartDate());
							awdBudgetPeriod.setEndDate(awdBudgetPeriod.getEndDate());
							awdBudgetPeriod.setBudgetId(awdBudgetHeader.getBudgetId());
							awdBudgetPeriod.setAwardNumber(vo.getAwardNumber());
							awdBudgetPeriod.setVersionNumber(awdBudgetHeader.getVersionNumber());
							awdBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
							awdBudgetPeriod.setUpdateUser(vo.getUserName());
							awdBudgetPeriod.setDevProposalId(vo.getPdNumber());
							awdBudgetPeriod.setDevProposalBudgetPeriod(vo.getDevPropBudgetPeriodNumber());
							awdBudgetPeriod.setDevProposalBudgetId(vo.getDevPropBudgetHeaderId());
							periodTotalCost = periodTotalCost.add(budgetPeriod.getTotalFundRequested());
							periodTotalDirectCost = periodTotalDirectCost.add(budgetPeriod.getTotalDirectCost());
							periodTotalIndirectCost = periodTotalIndirectCost.add(budgetPeriod.getTotalIndirectCost());
							List<AwardBudgetDetail> awdBudgetItemDetails = new ArrayList<>();
							awdBudgetPeriod = getAllAwardBudgetDetails(budgetPeriod.getBudgetDetails(), awdBudgetPeriod,
									awdBudgetItemDetails, awdBudgetHeader.getAwardId());
							awdBudgetPeriod.setTotalCost(periodTotalCost);
							awdBudgetPeriod.setTotalDirectCost(periodTotalDirectCost);
							awdBudgetPeriod.setTotalIndirectCost(periodTotalIndirectCost);
							newBudgetPeriods.add(awdBudgetPeriod);
						}
					}
					awdBudgetHeader.getBudgetPeriods().addAll(newBudgetPeriods);
				}
			} else {
				AwardBudgetPeriod awdBudgetPeriod = awdBudgetHeader.getBudgetPeriods().get(0);
				if (awdBudgetPeriod.getBudgetDetails() != null && !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
					awdBudgetPeriod = new AwardBudgetPeriod();
				}
				BigDecimal periodTotalCost = BigDecimal.ZERO;
				BigDecimal periodTotalDirectCost = BigDecimal.ZERO;
				BigDecimal periodTotalIndirectCost = BigDecimal.ZERO;
				awdBudgetPeriod.setStartDate(awdBudgetHeader.getStartDate());
				awdBudgetPeriod.setEndDate(awdBudgetHeader.getEndDate());
				awdBudgetPeriod.setBudgetId(awdBudgetHeader.getBudgetId());
				awdBudgetPeriod.setAwardNumber(vo.getAwardNumber());
				awdBudgetPeriod.setVersionNumber(awdBudgetHeader.getVersionNumber());
				awdBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awdBudgetPeriod.setUpdateUser(vo.getUserName());
				awdBudgetPeriod.setDevProposalId(vo.getPdNumber());
				awdBudgetPeriod.setDevProposalBudgetPeriod(vo.getDevPropBudgetPeriodNumber());
				awdBudgetPeriod.setDevProposalBudgetId(vo.getDevPropBudgetHeaderId());
				for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
					periodTotalCost = periodTotalCost.add(budgetPeriod.getTotalFundRequested());
					periodTotalDirectCost = periodTotalDirectCost.add(budgetPeriod.getTotalDirectCost());
					periodTotalIndirectCost = periodTotalIndirectCost.add(budgetPeriod.getTotalIndirectCost());
					awdBudgetPeriod.setBudgetPeriod(1);
					List<AwardBudgetDetail> awdBudgetItemDetails = new ArrayList<>();
					awdBudgetPeriod = getAllAwardBudgetDetails(budgetPeriod.getBudgetDetails(), awdBudgetPeriod,
							awdBudgetItemDetails,awdBudgetHeader.getAwardId());
				}
				awdBudgetPeriod.setTotalCost(periodTotalCost);
				awdBudgetPeriod.setTotalDirectCost(periodTotalDirectCost);
				awdBudgetPeriod.setTotalIndirectCost(periodTotalIndirectCost);
				awdBudgetHeader.getBudgetPeriods().clear();
				awdBudgetHeader.getBudgetPeriods().add(awdBudgetPeriod);
			}
		} else {
			if (Boolean.FALSE.equals(isSinglePeriodAward)) {
				for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
					if (budgetPeriod.getBudgetPeriodId().equals(vo.getDevPropBudgetPeriodId())) {
						Integer periodNumber = vo.getDevPropBudgetPeriodNumber() - 1;
						AwardBudgetPeriod awdBudgetPeriod = awdBudgetHeader.getBudgetPeriods().get(periodNumber);
						if (awdBudgetPeriod.getBudgetDetails() != null
								&& !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
							awdBudgetPeriod = new AwardBudgetPeriod();
						}
						awdBudgetPeriod.setTotalCost(budgetPeriod.getTotalFundRequested());
						awdBudgetPeriod.setTotalDirectCost(budgetPeriod.getTotalDirectCost());
						awdBudgetPeriod.setTotalIndirectCost(budgetPeriod.getTotalIndirectCost());
						awdBudgetPeriod.setBudgetPeriod(awdBudgetPeriod.getBudgetPeriod());
						awdBudgetPeriod.setStartDate(awdBudgetPeriod.getStartDate());
						awdBudgetPeriod.setEndDate(awdBudgetPeriod.getEndDate());
						awdBudgetPeriod.setBudgetId(awdBudgetHeader.getBudgetId());
						awdBudgetPeriod.setAwardNumber(vo.getAwardNumber());
						awdBudgetPeriod.setVersionNumber(awdBudgetHeader.getVersionNumber());
						awdBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						awdBudgetPeriod.setUpdateUser(vo.getUserName());
						awdBudgetPeriod.setDevProposalId(vo.getPdNumber());
						awdBudgetPeriod.setDevProposalBudgetPeriod(vo.getDevPropBudgetPeriodNumber());
						awdBudgetPeriod.setDevProposalBudgetId(vo.getDevPropBudgetHeaderId());
						awdBudgetPeriod = getAwardBudgetDetails(budgetPeriod.getBudgetDetails(), awdBudgetPeriod, awdBudgetHeader.getAwardId());
						awdBudgetHeader.getBudgetPeriods().clear();
						awdBudgetHeader.getBudgetPeriods().add(awdBudgetPeriod);
					}
				}
			} else {
				for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
					if (budgetPeriod.getBudgetPeriodId().equals(vo.getDevPropBudgetPeriodId())) {
						AwardBudgetPeriod awdBudgetPeriod = awdBudgetHeader.getBudgetPeriods().get(0);
						if (awdBudgetPeriod.getBudgetDetails() != null
								&& !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
							awdBudgetPeriod = new AwardBudgetPeriod();
						}
						awdBudgetPeriod.setTotalCost(budgetPeriod.getTotalFundRequested());
						awdBudgetPeriod.setTotalDirectCost(budgetPeriod.getTotalDirectCost());
						awdBudgetPeriod.setTotalIndirectCost(budgetPeriod.getTotalIndirectCost());
						awdBudgetPeriod.setBudgetPeriod(budgetPeriod.getBudgetPeriod());
						awdBudgetPeriod.setStartDate(awdBudgetHeader.getStartDate());
						awdBudgetPeriod.setEndDate(awdBudgetHeader.getEndDate());
						awdBudgetPeriod.setBudgetId(awdBudgetHeader.getBudgetId());
						awdBudgetPeriod.setAwardNumber(vo.getAwardNumber());
						awdBudgetPeriod.setVersionNumber(awdBudgetHeader.getVersionNumber());
						awdBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
						awdBudgetPeriod.setUpdateUser(vo.getUserName());
						awdBudgetPeriod.setDevProposalId(vo.getPdNumber());
						awdBudgetPeriod.setDevProposalBudgetPeriod(vo.getDevPropBudgetPeriodNumber());
						awdBudgetPeriod.setDevProposalBudgetId(vo.getDevPropBudgetHeaderId());
						awdBudgetPeriod = getAwardBudgetDetails(budgetPeriod.getBudgetDetails(), awdBudgetPeriod, awdBudgetHeader.getAwardId());
						awdBudgetHeader.getBudgetPeriods().clear();
						awdBudgetHeader.getBudgetPeriods().add(awdBudgetPeriod);
					}
				}
			}
		}
		awdBudgetHeader.setUpdateUser(vo.getUserName());
		awdBudgetHeader.setUpdateUserName(vo.getUserFullName());
		awdBudgetHeader.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		vo.setAwardBudgetHeader(awdBudgetHeader);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUserName());
		return commonDao.convertObjectToJSON(calculateAwardBudgetOverView(awdBudgetHeader.getBudgetId()));
	}

	private void saveBudgetPeriodData(List<AwardBudgetPeriod> periods, Integer budgetId) {
		for (AwardBudgetPeriod budgetPeriod : periods) {
			budgetPeriod.setBudgetId(budgetId);
			awardBudgetDao.saveBudgetPeriod(budgetPeriod);
			List<AwardBudgetDetail> details = budgetPeriod.getBudgetDetails();
			for (AwardBudgetDetail awardBudgetDetail : details) {
				awardBudgetDetail.setBudgetId(budgetId);
				awardBudgetDetail.setBudgetPeriodId(budgetPeriod.getBudgetPeriodId());
				awardBudgetDao.saveBudgetDetail(awardBudgetDetail);
			}
		}
	}

	public AwardBudgetPeriod getAwardBudgetDetails(List<BudgetDetail> budgetDetail, AwardBudgetPeriod awdBudgetPeriod,Integer awardId) {
		awdBudgetPeriod.getBudgetDetails().clear();
		for (BudgetDetail propBudgetDetail : budgetDetail) {
			AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
			awardBudgetDetail.setAwardNumber(awdBudgetPeriod.getAwardNumber());
			awardBudgetDetail.setVersionNumber(awdBudgetPeriod.getVersionNumber());
			awardBudgetDetail.setBudgetCategoryCode(propBudgetDetail.getBudgetCategoryCode());
			awardBudgetDetail.setBudgetCategory(propBudgetDetail.getBudgetCategory());
			awardBudgetDetail.setCostElementCode(propBudgetDetail.getCostElementCode());
			awardBudgetDetail.setCostElement(propBudgetDetail.getCostElement());
			awardBudgetDetail.setLineItemDescription(propBudgetDetail.getLineItemDescription());
			awardBudgetDetail.setLineItemCost(propBudgetDetail.getSponsorRequestedAmount());
			awardBudgetDetail.setLineItemDescription(propBudgetDetail.getLineItemDescription());
			if (propBudgetDetail.getQuantity() != null) {
				awardBudgetDetail.setQuantity(propBudgetDetail.getQuantity());
			}
			awardBudgetDetail.setIsSystemGeneratedCostElement(propBudgetDetail.getIsSystemGeneratedCostElement());
			awardBudgetDetail.setSystemGeneratedCEType(propBudgetDetail.getSystemGeneratedCEType());
			awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetDetail.setUpdateUser(awdBudgetPeriod.getUpdateUser());
			awardBudgetDetail.setBudgetPeriod(awdBudgetPeriod.getBudgetPeriod());
			awardBudgetDetail.setBudgetPeriodId(awdBudgetPeriod.getBudgetPeriodId());
			awardBudgetDetail.setBudgetId(awdBudgetPeriod.getBudgetId());
			awardBudgetDetail.getPersonsDetails()
					.addAll(copyPersonFromProposalBudget(propBudgetDetail, awardBudgetDetail, awdBudgetPeriod));
			awdBudgetPeriod.getBudgetDetails().add(awardBudgetDetail);
		}
		for (AwardBudgetDetail awardBudgetDetail : awdBudgetPeriod.getBudgetDetails()) {
			awardBudgetDao.saveBudgetDetail(awardBudgetDetail);
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION) && awardDao.getAccountNumberByAwardId(awardId) != null && awardBudgetDetail.getInternalOrderCode() == null) {
				wbsDao.generateWBSNumber(awardId, "N", awardBudgetDetail.getBudgetDetailId(), awardBudgetDetail.getBudgetCategoryCode());
			}
			awardBudgetDetail.setInternalOrderCode(awardBudgetDao.getInternalOrderCodeByBudgetDetailId(awardBudgetDetail.getBudgetDetailId()));
		}
		return awdBudgetPeriod;
	}

	@Override
	public String getAwardBudgetVersionsByAwardId(AwardBudgetVO vo) {
		try {
			List<AwardBudgetHeader> awdBudgetHeader = awardBudgetDao.getAwardBudgetVersionsByAwardId(vo.getAwardId());
			/*List<AwardBudgetHeader> awardBudgetHeaders = new ArrayList<>();
			for (AwardBudgetHeader awardBudgetHeader : awdBudgetHeader) {
				AwardBudgetHeader budgetHeader = new AwardBudgetHeader();
				budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(awardBudgetHeader.getBudgetId());
				fetchAwardBudgetPeriods(budgetHeader);
				awardBudgetHeaders.add(budgetHeader);
			}*/
			if (!awdBudgetHeader.isEmpty()) {
				List<AwardBudgetHeaderDetail> awardBudgetHeaderDetails = new ArrayList<>();
				for (AwardBudgetHeader awardBudgetHeader : awdBudgetHeader) {
					awardBudgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(awardBudgetHeader));
				}
				vo.setAwardBudgetList(awardBudgetHeaderDetails);
			}
			vo.setIsCreated(true);
		} catch (Exception e) {
			logger.info("error in get Award Budget Versions By AwardId", e);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void setAwardBudgetHeaderBasedOnVersionNumber(AwardBudgetHeader awardBudgetHeader, AwardBudgetVO vo) {
		List<AwardBudgetPeriod> updatedAwardBudgetPeriods = new ArrayList<>();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
			List<AwardBudgetDetail> updatedAwardBudgetDetails = new ArrayList<>();
			for (AwardBudgetDetail awardBudgetDetail : awardBudgetPeriod.getBudgetDetails()) {
				calculateBalanceToDateValue(awardBudgetDetail, awardBudgetHeader.getFundCode());
				updatedAwardBudgetDetails.add(awardBudgetDetail);
			}
			awardBudgetPeriod.getBudgetDetails().clear();
			awardBudgetPeriod.getBudgetDetails().addAll(updatedAwardBudgetDetails);
			updatedAwardBudgetPeriods.add(awardBudgetPeriod);
		}
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(updatedAwardBudgetPeriods);
		vo.setAwardBudgetHeader(awardBudgetHeader);
	}

	public AwardBudgetPeriod getAllAwardBudgetDetails(List<BudgetDetail> budgetDetails,
			AwardBudgetPeriod awdBudgetPeriod, List<AwardBudgetDetail> awdBudgetItemDetails, Integer awardId) {
		BigDecimal lineItemCost;
		if (awdBudgetPeriod.getBudgetDetails() != null && !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
			List<String> abCostElements = new ArrayList<>();
			List<AwardBudgetDetail> awdBudgetDetails = awdBudgetPeriod.getBudgetDetails();
			for (AwardBudgetDetail awdBudgetDetail : awdBudgetDetails) {
				abCostElements.add(awdBudgetDetail.getCostElementCode());
			}
			for (AwardBudgetDetail awdBudgetDetail : awdBudgetDetails) {
				for (BudgetDetail bdgtDetail : budgetDetails) {
					if (awdBudgetDetail.getCostElementCode().equals(bdgtDetail.getCostElementCode())) {
						lineItemCost = awdBudgetDetail.getLineItemCost().add(bdgtDetail.getSponsorRequestedAmount());
						awdBudgetDetail.setLineItemCost(lineItemCost);
					} else {
						if (!(abCostElements.contains(bdgtDetail.getCostElementCode()))) {
							AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
							awardBudgetDetail.setAwardNumber(awdBudgetPeriod.getAwardNumber());
							awardBudgetDetail.setVersionNumber(awdBudgetPeriod.getVersionNumber());
							awardBudgetDetail.setBudgetCategoryCode(bdgtDetail.getBudgetCategoryCode());
							awardBudgetDetail.setBudgetCategory(bdgtDetail.getBudgetCategory());
							awardBudgetDetail.setCostElementCode(bdgtDetail.getCostElementCode());
							awardBudgetDetail.setCostElement(bdgtDetail.getCostElement());
							awardBudgetDetail.setLineItemCost(bdgtDetail.getSponsorRequestedAmount());
							awardBudgetDetail.setLineItemNumber(bdgtDetail.getLineItemNumber());
							awardBudgetDetail.setLineItemDescription(bdgtDetail.getLineItemDescription());
							if (bdgtDetail.getQuantity() != null) {
								awardBudgetDetail.setQuantity(bdgtDetail.getQuantity());
							}
							awardBudgetDetail
									.setIsSystemGeneratedCostElement(bdgtDetail.getIsSystemGeneratedCostElement());
							awardBudgetDetail.setSystemGeneratedCEType(bdgtDetail.getSystemGeneratedCEType());
							awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
							awardBudgetDetail.setUpdateUser(awdBudgetPeriod.getUpdateUser());
							awardBudgetDetail.setBudgetId(awdBudgetPeriod.getBudgetId());
							awardBudgetDetail.setBudgetPeriod(awdBudgetPeriod.getBudgetPeriod());
							awardBudgetDetail.setBudgetPeriodId(awdBudgetPeriod.getBudgetPeriodId());
							awardBudgetDetail.getPersonsDetails()
									.addAll(copyPersonFromProposalBudget(bdgtDetail, awdBudgetDetail, awdBudgetPeriod));
							awdBudgetItemDetails.add(awardBudgetDetail);
							abCostElements.add(awardBudgetDetail.getCostElementCode());
						}
					}
				}
			}
			awdBudgetPeriod.getBudgetDetails().addAll(awdBudgetItemDetails);
		} else {
			for (BudgetDetail propBudgetDetail : budgetDetails) {
				AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
				awardBudgetDetail.setAwardNumber(awdBudgetPeriod.getAwardNumber());
				awardBudgetDetail.setLineItemNumber(propBudgetDetail.getLineItemNumber());
				awardBudgetDetail.setVersionNumber(awdBudgetPeriod.getVersionNumber());
				awardBudgetDetail.setBudgetCategoryCode(propBudgetDetail.getBudgetCategoryCode());
				awardBudgetDetail.setBudgetCategory(propBudgetDetail.getBudgetCategory());
				awardBudgetDetail.setCostElementCode(propBudgetDetail.getCostElementCode());
				awardBudgetDetail.setCostElement(propBudgetDetail.getCostElement());
				awardBudgetDetail.setLineItemDescription(propBudgetDetail.getLineItemDescription());
				awardBudgetDetail.setLineItemCost(propBudgetDetail.getSponsorRequestedAmount());
				if (propBudgetDetail.getQuantity() != null) {
					awardBudgetDetail.setQuantity(propBudgetDetail.getQuantity());
				}
				awardBudgetDetail.setLineItemDescription(propBudgetDetail.getLineItemDescription());
				awardBudgetDetail.setIsSystemGeneratedCostElement(propBudgetDetail.getIsSystemGeneratedCostElement());
				awardBudgetDetail.setSystemGeneratedCEType(propBudgetDetail.getSystemGeneratedCEType());
				awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardBudgetDetail.setUpdateUser(awdBudgetPeriod.getUpdateUser());
				awardBudgetDetail.setBudgetPeriod(awdBudgetPeriod.getBudgetPeriod());
				awardBudgetDetail.setBudgetPeriodId(awdBudgetPeriod.getBudgetPeriodId());
				awardBudgetDetail.setBudgetId(awdBudgetPeriod.getBudgetId());
				awardBudgetDetail.getPersonsDetails()
						.addAll(copyPersonFromProposalBudget(propBudgetDetail, awardBudgetDetail, awdBudgetPeriod));
				awdBudgetItemDetails.add(awardBudgetDetail);
			}
			for (AwardBudgetDetail awardBudgetDetail : awdBudgetItemDetails) {
				awardBudgetDao.saveBudgetDetail(awardBudgetDetail);
				if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION) && awardDao.getAccountNumberByAwardId(awardId) != null && awardBudgetDetail.getInternalOrderCode() == null) {
					wbsDao.generateWBSNumber(awardId, "N", awardBudgetDetail.getBudgetDetailId(), awardBudgetDetail.getBudgetCategoryCode());
				}
				awardBudgetDetail.setInternalOrderCode(awardBudgetDao.getInternalOrderCodeByBudgetDetailId(awardBudgetDetail.getBudgetDetailId()));
			}
			awdBudgetPeriod.getBudgetDetails().addAll(awdBudgetItemDetails);
		}
		return awdBudgetPeriod;
	}

	private List<AwardBudgetPersonalDetail> copyPersonFromProposalBudget(BudgetDetail budgetDetail,
			AwardBudgetDetail awdBudgetDetail, AwardBudgetPeriod awdBudgetPeriod) {
		List<AwardBudgetPersonalDetail> awardBudgetPersonalDetails = new ArrayList<>();
		for (BudgetPersonalDetails personsDetails : budgetDetail.getPersonsDetails()) {
			AwardBudgetPersonalDetail personDetails = new AwardBudgetPersonalDetail();
			AwardBudgetPerson awardBudgetPerson = copyBudgetPersonFromProposalBudget(personsDetails, awdBudgetPeriod);
			personDetails.setBudgetDetail(awdBudgetDetail);
			personDetails.setBudgetPersonDetailId(null);
			personDetails.setBudgetPerson(awardBudgetPerson);
			personDetails.setSalaryRequested(personsDetails.getSalaryRequested());
			personDetails.setTotalSalary(personsDetails.getSalary());
			personDetails.setStartDate(awdBudgetPeriod.getStartDate());
			personDetails.setEndDate(awdBudgetPeriod.getEndDate());
			personDetails.setPercentageEffort(personsDetails.getPercentageEffort());
			personDetails.setBudgetPersonId(awardBudgetPerson.getBudgetPersonId());
			awardBudgetPersonalDetails.add(personDetails);
		}
		return awardBudgetPersonalDetails;
	}

	private AwardBudgetPerson copyBudgetPersonFromProposalBudget(BudgetPersonalDetails personsDetails,
			AwardBudgetPeriod awdBudgetPeriod) {
		AwardBudgetPerson awardBudgetPerson = new AwardBudgetPerson();
		try {
			BudgetPerson budgetPerson = proposalBudgetDao.getBudgetPersonByPersonId(personsDetails.getBudgetPersonId());
			Boolean isPersonExist = awardBudgetDao.checkBudgetPerson(awdBudgetPeriod.getBudgetId(),
					budgetPerson.getPersonId(), budgetPerson.getRolodexId(), budgetPerson.getPersonType(),
					budgetPerson.getTbnId());
			if (Boolean.TRUE.equals(isPersonExist)) {
				AwardBudgetPerson awardBudgetPersons = awardBudgetDao.getBudgetPersonByPersonId(
						awdBudgetPeriod.getBudgetId(), budgetPerson.getPersonId(), budgetPerson.getRolodexId(),
						budgetPerson.getPersonType(), budgetPerson.getTbnId());
				if (awardBudgetPersons != null) {
					awardBudgetPerson.setBudgetPersonId(awardBudgetPersons.getBudgetPersonId());
				}
			}
			awardBudgetPerson.setAppointmentType(budgetPerson.getAppointmentType());
			awardBudgetPerson.setAppointmentTypeCode(budgetPerson.getAppointmentTypeCode());
			awardBudgetPerson.setBudgetHeaderId(awdBudgetPeriod.getBudgetId());
			awardBudgetPerson.setCalculationBase(budgetPerson.getCalculationBase());
			awardBudgetPerson.setDurationCost(budgetPerson.getDurationCost());
			awardBudgetPerson.setEffectiveDate(budgetPerson.getEffectiveDate());
			awardBudgetPerson.setJobCode(budgetPerson.getJobCodeType());
			awardBudgetPerson.setJobCodes(budgetPerson.getJobCode());
			awardBudgetPerson.setPersonName(budgetPerson.getPersonName());
			awardBudgetPerson.setPersonType(budgetPerson.getPersonType());
			awardBudgetPerson.setSalaryAnniversaryDate(budgetPerson.getSalaryAnniversaryDate());
			awardBudgetPerson.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudgetPerson.setUpdateUser(budgetPerson.getUpdateUser());
			awardBudgetPerson.setPersonId(budgetPerson.getPersonId());
			awardBudgetPerson.setTbnId(budgetPerson.getTbnId());
			awardBudgetPerson.setRolodexId(budgetPerson.getRolodexId());
			awardBudgetDao.saveOrUpdateAwardBudgetPerson(awardBudgetPerson);
		} catch (Exception e) {
			logger.info("error in copyBudgetPersonFromProposalBudget", e);
		}
		return awardBudgetPerson;
	}

	@Override
	public void calculateBalanceToDateValue(AwardBudgetDetail awardBudgetDetail, String fundCode) {
		AwardExpenseDetail awardExpenseDetail = awardExpenseDao.fetchAwardExpenseDetailByParams(awardBudgetDetail.getAwardNumber(), fundCode, awardBudgetDetail.getInternalOrderCode());
		AwardExpenseDetailsExt awardExpenseDetailsExt = awardExpenseDao.getAwardExpenseExtDetailsByParams(awardBudgetDetail.getAwardNumber(), fundCode, awardBudgetDetail.getInternalOrderCode());
		BigDecimal committedAmount = BigDecimal.ZERO;
		BigDecimal totalExpenseAmount = BigDecimal.ZERO;
		if (awardExpenseDetailsExt != null && awardExpenseDetailsExt.getCommittedAmount() != null) {
			committedAmount = awardExpenseDetailsExt.getCommittedAmount();
		}
		if (awardExpenseDetail != null && awardExpenseDetail.getTotalExpenseAmount() != null) {
			totalExpenseAmount = awardExpenseDetail.getTotalExpenseAmount();
		}
		if (awardBudgetDetail.getPrevLineItemCost() != null) {
			awardBudgetDetail.setBalanceToDate(awardBudgetDetail.getPrevLineItemCost().subtract(totalExpenseAmount.add(committedAmount)));
		}
	}

	public AwardBudgetHeaderDetail prepareAwardBudgetHeaderDetail(AwardBudgetHeader awardBudgetHeader) {
		AwardBudgetHeaderDetail awardBudgetHeaderDetail = new AwardBudgetHeaderDetail();
		awardBudgetHeaderDetail.setAwardId(awardBudgetHeader.getAwardId());
		awardBudgetHeaderDetail.setAwardNumber(awardBudgetHeader.getAwardNumber());
		awardBudgetHeaderDetail.setBudgetId(awardBudgetHeader.getBudgetId());
		awardBudgetHeaderDetail.setBudgetStatus(awardBudgetHeader.getBudgetStatus().getDescription());
		if (awardBudgetHeader.getBudgetType() != null) {
			awardBudgetHeaderDetail.setBudgetType(awardBudgetHeader.getBudgetType().getDescription());
		}
		awardBudgetHeaderDetail.setEndDate(awardBudgetHeader.getEndDate());
		awardBudgetHeaderDetail.setfAndARateType(awardBudgetHeader.getRateType().getDescription());
		awardBudgetHeaderDetail.setOnOffCampusFlag(awardBudgetHeader.getOnOffCampusFlag());
		awardBudgetHeaderDetail.setSequenceNumber(awardBudgetHeader.getSequenceNumber());
		awardBudgetHeaderDetail.setStartDate(awardBudgetHeader.getStartDate());
		awardBudgetHeaderDetail.setTotalCost(awardBudgetHeader.getTotalCost());
		awardBudgetHeaderDetail.setTotalDirectCost(awardBudgetHeader.getTotalDirectCost());
		awardBudgetHeaderDetail.setTotalIndirectCost(awardBudgetHeader.getTotalIndirectCost());
		awardBudgetHeaderDetail.setUpdateTimeStamp(awardBudgetHeader.getUpdateTimeStamp());
		awardBudgetHeaderDetail.setUpdateUserName(personDao.getUserFullNameByUserName(awardBudgetHeader.getUpdateUser()));
		awardBudgetHeaderDetail.setVersionNumber(awardBudgetHeader.getVersionNumber());
		awardBudgetHeaderDetail.setBudgetStatusCode(awardBudgetHeader.getBudgetStatusCode());
		return awardBudgetHeaderDetail;
	}

	@Override
	public AwardBudgetHeader generateAwardBudgetFromProposalBudget(AwardBudgetVO vo) {
		fetchAwardBudgetPeriodsFromBudgetHeader(vo);
//		fetchAwardBudgetPeriods(vo.getAwardBudgetHeader());
		AwardBudgetHeader awdBudgetHeader = vo.getAwardBudgetHeader();
		BudgetHeader budgetHeader = vo.getBudgetHeader();
		AwardBudgetPeriod awdBudgetPeriod =  awdBudgetHeader.getBudgetPeriods() != null &&  !awdBudgetHeader.getBudgetPeriods().isEmpty() ? awdBudgetHeader.getBudgetPeriods().get(0) : new AwardBudgetPeriod();
		if (awdBudgetPeriod.getBudgetDetails() != null && !awdBudgetPeriod.getBudgetDetails().isEmpty()) {
			awdBudgetPeriod = new AwardBudgetPeriod();
		}
		BigDecimal periodTotalCost = BigDecimal.ZERO;
		BigDecimal periodTotalDirectCost = BigDecimal.ZERO;
		BigDecimal periodTotalIndirectCost = BigDecimal.ZERO;
		awdBudgetPeriod.setStartDate(awdBudgetHeader.getStartDate());
		awdBudgetPeriod.setEndDate(awdBudgetHeader.getEndDate());
		awdBudgetPeriod.setBudgetId(awdBudgetHeader.getBudgetId());

		awdBudgetPeriod.setAwardNumber(vo.getAwardNumber());
		awdBudgetPeriod.setVersionNumber(awdBudgetHeader.getVersionNumber());
		awdBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awdBudgetPeriod.setUpdateUser(vo.getUserName());
		awdBudgetPeriod.setDevProposalId(vo.getPdNumber());
		awdBudgetPeriod.setDevProposalBudgetPeriod(vo.getDevPropBudgetPeriodNumber());
		awdBudgetPeriod.setDevProposalBudgetId(vo.getDevPropBudgetHeaderId());
		for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
			periodTotalCost = periodTotalCost.add(budgetPeriod.getTotalFundRequested());
			periodTotalDirectCost = periodTotalDirectCost.add(budgetPeriod.getTotalDirectCost());
			periodTotalIndirectCost = periodTotalIndirectCost.add(budgetPeriod.getTotalIndirectCost());
			List<AwardBudgetDetail> awdBudgetItemDetails = new ArrayList<>();
			awdBudgetPeriod = getAllAwardBudgetDetails(budgetPeriod.getBudgetDetails(), awdBudgetPeriod,
					awdBudgetItemDetails, awdBudgetHeader.getAwardId());
		}
		awdBudgetPeriod.setBudgetPeriod(1);
		awdBudgetPeriod.setTotalCost(periodTotalCost);
		awdBudgetPeriod.setTotalDirectCost(periodTotalDirectCost);
		awdBudgetPeriod.setTotalIndirectCost(periodTotalIndirectCost);
		awdBudgetHeader.getBudgetPeriods().clear();
		awdBudgetHeader.getBudgetPeriods().add(awdBudgetPeriod);
		saveBudgetPeriodData(awdBudgetHeader.getBudgetPeriods(), awdBudgetHeader.getBudgetId());
		calculateAwardBudgetPeriod(awdBudgetHeader);
		awardBudgetDao.saveAwardBudgetHeader(awdBudgetHeader);
		String defaultAbPersonItemToBeTBN = commonDao.getParameterValueAsString(Constants.DEFAULT_AB_TBN_PERSON_ID);
		if (defaultAbPersonItemToBeTBN != null && !defaultAbPersonItemToBeTBN.isEmpty()) {
			createDefaultAwardBudgetPersonItem(awdBudgetHeader.getBudgetId(), awdBudgetHeader.getUpdateUser(), defaultAbPersonItemToBeTBN);
		}
		return awdBudgetHeader;
	}

	private List<AwardBudgetPeriod> fetchAwardBudgetPeriodsFromBudgetHeader(AwardBudgetVO vo) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		AwardBudgetHeader awardBudgetHeader = vo.getAwardBudgetHeader();
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetPeriods.addAll(awardBudgetDao.getAwardBudgetPeriodsByBudgetId(vo.getBudgetHeader().getBudgetId()));
		for (AwardBudgetPeriod period : awardBudgetPeriods) {
			period.getBudgetDetails().clear();
			List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
			budgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(period.getBudgetPeriodId());
			for (AwardBudgetDetail details : budgetDetails) {
				details.setBudgetDetailCalcAmounts(awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(details.getBudgetDetailId()));
				details.setBudgetId(awardBudgetHeader.getBudgetId());
				if (commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED)) {
					setManpowerCommittedCost(details, awardBudgetHeader.getAwardId());
				}
				period.getBudgetDetails().add(details);
			}
			awardBudgetHeader.getBudgetPeriods().add(period);
		}
		return awardBudgetHeader.getBudgetPeriods();
		
	}

	@Override
	public AwardBudgetVO convertStringJSONToObject(String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		AwardBudgetVO vo = null;
		try {
			mapper.setVisibility(PropertyAccessor.FIELD, Visibility.ANY);
			vo = mapper.readValue(formDataJSON, AwardBudgetVO.class);
		} catch (JsonParseException jsonParseException) {
			logger.info("error in jsonParseException on convertStringJSONToObject", jsonParseException);
		} catch (JsonMappingException jsonMappingException) {
			logger.info("error in jsonMappingException on convertStringJSONToObject", jsonMappingException);
		} catch (IOException iOException) {
			logger.info("error in iOException on convertStringJSONToObject", iOException);
		}
		return vo;
	}

	@Override
	public String addAwardBudgetPeriod(Integer budgetId, String userName) {
		AwardBudgetVO awardBudgetVO = new AwardBudgetVO();
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
		fetchAwardBudgetPeriods(awardBudgetHeader);
		Integer awardId = awardBudgetHeader.getAwardId();
		List<AwardBudgetPeriod> awardBudgetPeriods = awardBudgetHeader.getBudgetPeriods();
		List<AwardBudgetPeriod> updateAwardBudgetPeriods = new ArrayList<>(awardBudgetPeriods);
		Collections.copy(updateAwardBudgetPeriods, awardBudgetPeriods);
		AwardBudgetPeriod lastPeriod = awardBudgetDao.getMaxBudgetPeriodByBudgetId(budgetId);
		AwardBudgetPeriod newAwardBudgetPeriod = new AwardBudgetPeriod();
		newAwardBudgetPeriod.setAwardNumber(awardBudgetHeader.getAwardNumber());
		newAwardBudgetPeriod.setBudgetPeriod(lastPeriod.getBudgetPeriod() + 1);
		newAwardBudgetPeriod.setBudgetId(awardBudgetHeader.getBudgetId());
		newAwardBudgetPeriod.setSubcontractCost(BigDecimal.ZERO);
		newAwardBudgetPeriod.setTotalCost(BigDecimal.ZERO);
		newAwardBudgetPeriod.setTotalDirectCost(BigDecimal.ZERO);
		newAwardBudgetPeriod.setTotalIndirectCost(BigDecimal.ZERO);
		newAwardBudgetPeriod.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		newAwardBudgetPeriod.setUpdateUser(awardBudgetHeader.getUpdateUserName());
		newAwardBudgetPeriod = awardBudgetDao.saveBudgetPeriod(newAwardBudgetPeriod);
		updateAwardBudgetPeriods.add(newAwardBudgetPeriod);
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(updateAwardBudgetPeriods);
		awardBudgetHeader.setInitialAvailableFund(calculateAvailableFund(awardBudgetHeader.getAwardId(),awardBudgetHeader.getAwardNumber(), true, awardBudgetHeader.getAvailableFundType()));
		awardBudgetHeader.setAvailableFund(awardBudgetHeader.getInitialAvailableFund().subtract(awardBudgetHeader.getTotalCost()));
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		awardBudgetHeader.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		awardBudgetHeader.setManpowerEnabled(manpowerEnabled);
		awardBudgetHeader.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
		awardBudgetVO.setAwardBudgetHeader(awardBudgetHeader);
		List<AwardBudgetHeaderDetail> awardBudgetHeaderDetails = new ArrayList<>();
		List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(awardId);
		for (AwardBudgetHeader budget : awardBudgetHeaders) {
			awardBudgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
		}
		awardBudgetVO.setAwardBudgetList(awardBudgetHeaderDetails);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardBudgetHeader.getAwardId(), userName);
		return commonDao.convertObjectToJSON(awardBudgetVO);
	}

	@Override
	public String copyAwardBudgetPeriod(AwardBudgetVO vo) {
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		fetchAwardBudgetPeriods(budgetHeader);
		List<AwardBudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		AwardBudgetPeriod copyPeriod = awardBudgetDao.getPeriodById(vo.getCopyPeriodId());
		for (AwardBudgetPeriod currentPeriod : budgetPeriods) {
			if (currentPeriod.getBudgetPeriodId().equals(vo.getCurrentPeriodId())) {
				awardBudgetDetails = copyBudgetDetails(copyPeriod, currentPeriod, vo.getUserName());
				if (awardBudgetDetails.isEmpty()) {
					currentPeriod.setTotalCost(copyPeriod.getTotalCost());
					currentPeriod.setTotalDirectCost(copyPeriod.getTotalDirectCost());
					currentPeriod.setTotalIndirectCost(copyPeriod.getTotalIndirectCost());
				}
			}
		}
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(vo.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			if (budget.getBudgetId().equals(budgetHeader.getBudgetId())) {
				budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budgetHeader));
			} else {
				budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
			}
		}
		vo.setAwardBudgetHeader(budgetHeader);
		vo.setAwardBudgetList(budgetHeaderDetails);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUserName());
		return commonDao.convertObjectToJSON(calculateCopyAwardBudget(vo, awardBudgetDetails));
	}

	private List<AwardBudgetDetailCalcAmount> copyAwardBudgetDetailCalcAmounts(AwardBudgetDetail budgetDetail) {
		AwardBudgetDetailCalcAmount budgetCalculatedAmount = new AwardBudgetDetailCalcAmount();
		List<AwardBudgetDetailCalcAmount> budgetDetailCalcAmounts = new ArrayList<>();
		List<AwardBudgetDetailCalcAmount> copiedBudgetDetailCalcAmounts = new ArrayList<>();
		budgetDetailCalcAmounts = awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(budgetDetail.getBudgetDetailId());
		for (AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount : budgetDetailCalcAmounts) {
			budgetCalculatedAmount = copyAwardBudgetCalculatedAmount(budgetDetail,awardBudgetDetailCalcAmount);
			copiedBudgetDetailCalcAmounts.add(budgetCalculatedAmount);
		}
		return copiedBudgetDetailCalcAmounts;
	}
	
	private AwardBudgetDetailCalcAmount copyAwardBudgetCalculatedAmount(AwardBudgetDetail budgetDetail,
			AwardBudgetDetailCalcAmount budgetDetailCalcAmounts) {
		AwardBudgetDetailCalcAmount  copyBudgetCalculatedAmount = new AwardBudgetDetailCalcAmount();
		copyBudgetCalculatedAmount.setBudgetId(budgetDetail.getBudgetId());
		copyBudgetCalculatedAmount.setBudgetPeriod(budgetDetail.getBudgetPeriod());
		copyBudgetCalculatedAmount.setBudgetPeriodId(budgetDetail.getBudgetPeriodId());
		copyBudgetCalculatedAmount.setLineItemNumber(budgetDetail.getLineItemNumber());
		copyBudgetCalculatedAmount.setRateClassCode(budgetDetailCalcAmounts.getRateClassCode());
		copyBudgetCalculatedAmount.setRateClass(budgetDetailCalcAmounts.getRateClass());
		copyBudgetCalculatedAmount.setRateTypeCode(budgetDetailCalcAmounts.getRateTypeCode());
		copyBudgetCalculatedAmount.setRateType(budgetDetailCalcAmounts.getRateType());
		copyBudgetCalculatedAmount.setApplyRateFlag(true);
		copyBudgetCalculatedAmount.setRateTypeDescription(budgetDetailCalcAmounts.getRateType().getDescription());
		copyBudgetCalculatedAmount.setBudgetDetailId(budgetDetail.getBudgetDetailId());
		copyBudgetCalculatedAmount.setApplicableRate(budgetDetailCalcAmounts.getApplicableRate());
		copyBudgetCalculatedAmount.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyBudgetCalculatedAmount.setUpdateUser(budgetDetail.getUpdateUser());
		return awardBudgetDao.saveOrUpdateAwardBudgetDetailCalcAmount(copyBudgetCalculatedAmount);
	}

	private List<AwardBudgetDetail> copyBudgetDetails(AwardBudgetPeriod copyPeriod,
			AwardBudgetPeriod currentPeriod, String userName) {
		List<AwardBudgetDetail> newLineItems = new ArrayList<>();
		List<AwardBudgetDetail> budgetDetails = copyPeriod.getBudgetDetails();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			List<AwardBudgetDetail> copiedBudgetDetails = new ArrayList<>(budgetDetails);
			Collections.copy(copiedBudgetDetails, budgetDetails);
			for (AwardBudgetDetail budgetDetail : copiedBudgetDetails) {
				AwardBudgetDetail detail = new AwardBudgetDetail();
				detail.setAwardNumber(copyPeriod.getAwardNumber());
				detail.setBalanceToDate(budgetDetail.getBalanceToDate());
				detail.setBudgetCategory(budgetDetail.getBudgetCategory());
				detail.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
				detail.setBudgetDetailCalcAmounts(copyAwardBudgetDetailCalcAmounts(budgetDetail));
				detail.setBudgetDetailId(null);
				detail.setBudgetJustification(budgetDetail.getBudgetJustification());
				detail.setCostElement(budgetDetail.getCostElement());
				detail.setCostElementCode(budgetDetail.getCostElementCode());
				detail.setCostSharingAmount(budgetDetail.getCostSharingAmount());
				detail.setCostSharingPercentage(budgetDetail.getCostSharingPercentage());
				detail.setEndDate(budgetDetail.getEndDate());
				detail.setInternalOrderCode(budgetDetail.getInternalOrderCode());
				detail.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
				detail.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
				detail.setLineItemCost(budgetDetail.getLineItemCost());
				detail.setLineItemDescription(budgetDetail.getLineItemDescription());
				detail.setLineItemNumber(budgetDetail.getLineItemNumber());
				detail.setOnOffCampusFlag(budgetDetail.getOnOffCampusFlag());
				detail.setBudgetPeriodId(currentPeriod.getBudgetPeriodId());
				detail.setPrevLineItemCost(budgetDetail.getPrevLineItemCost());
				detail.setQuantity(budgetDetail.getQuantity());
				detail.setStartDate(budgetDetail.getStartDate());
				detail.setSystemGeneratedCEType(budgetDetail.getSystemGeneratedCEType());
				detail.setTbnId(budgetDetail.getTbnId());
				detail.setTbnPerson(budgetDetail.getTbnPerson());
				detail.setUpdateTimeStamp(budgetDetail.getUpdateTimeStamp());
				detail.setUpdateUser(budgetDetail.getUpdateUser());
				detail.setBudgetId(budgetDetail.getBudgetId());
				detail.setBudgetPeriod(currentPeriod.getBudgetPeriod());
				detail.setPersonsDetails(
						copyBudgetPersonDetails(budgetDetail.getPersonsDetails(), detail, currentPeriod));
				detail = awardBudgetDao.saveOrUpdateAwardBudgetLineItem(detail);
				newLineItems.add(detail);
			}
			currentPeriod.getBudgetDetails().clear();
			currentPeriod.getBudgetDetails().addAll(newLineItems);
		}
		return newLineItems;
	}

	@Override
	public String generateAwardBudgetPeriods(AwardBudgetVO vo) {
		AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		List<AwardBudgetDetail> awdDetails = new ArrayList<>();
		fetchAwardBudgetPeriods(budgetHeader);
		awardBudgetDao.saveBudgetHeader(budgetHeader);
		List<AwardBudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		AwardBudgetPeriod copyPeriod = awardBudgetDao.getPeriodById(vo.getCopyPeriodId());
		for (AwardBudgetPeriod currentPeriod : budgetPeriods) {
			currentPeriod.setAwardNumber(budgetHeader.getAwardNumber());
			if (!currentPeriod.getBudgetPeriodId().equals(vo.getCopyPeriodId())) {
				awdDetails = copyBudgetDetails(copyPeriod, currentPeriod, vo.getUserName());
				if (awdDetails.isEmpty()) {
					currentPeriod.setTotalCost(copyPeriod.getTotalCost());
					currentPeriod.setTotalDirectCost(copyPeriod.getTotalDirectCost());
					currentPeriod.setTotalIndirectCost(copyPeriod.getTotalIndirectCost());
				}
			}
		}
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(vo.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
		}
		vo.setAwardBudgetHeader(budgetHeader);
		vo.setAwardBudgetList(budgetHeaderDetails);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(budgetHeader.getAwardId(), vo.getUserName());
		return commonDao.convertObjectToJSON(calculateAllAwardBudget(budgetHeader.getBudgetId(), vo.getIsEnabledAwardBudgetDetail()));
	}

	public AwardBudgetVO calculateAllAwardBudget(Integer BudgetId, boolean isBudgetDetailEmpty) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(BudgetId);
		if (awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			fetchAwardBudgetPeriods(awardBudgetHeader);
		}
		calculateAllAwardBudgetPeriod(awardBudgetHeader, isBudgetDetailEmpty);
		return calculateAwardBudgetHeader(awardBudgetHeader);
	}

	private void calculateAllAwardBudgetPeriod(AwardBudgetHeader awardBudgetHeader, boolean isBudgetDetailEmpty) {
		List<AwardBudgetPeriod> awardBudgetPeriodList = new ArrayList<>();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
			BigDecimal totalFringeCost = BigDecimal.ZERO;
			BigDecimal totalFandACost = BigDecimal.ZERO;
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			if (isBudgetDetailEmpty) {
				List<AwardBudgetDetail> budgetDetail = awardBudgetPeriod.getBudgetDetails();
				if (!budgetDetail.isEmpty() && budgetDetail != null) {
					for (AwardBudgetDetail awardBudgetDetail : budgetDetail) {
						if (awardBudgetPeriod.getBudgetPeriodId().equals(awardBudgetDetail.getBudgetPeriodId())) {
							if (awardBudgetDetail.getIsSystemGeneratedCostElement()) {
								if (Constants.BUDGET_FRINGE_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
										|| Constants.BUDGET_FRINGE_OFF
												.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
									totalFringeCost = totalFringeCost.add(awardBudgetDetail.getLineItemCost());
								}
								if (Constants.BUDGET_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
										|| Constants.BUDGET_OH_OFF
												.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
									totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
								}
								if (Constants.BUDGET_RESEARCH_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
									totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
								}
							} else {
								if (awardBudgetDetail.getLineItemCost() != null) {
									totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
								}
							}
						}
					}
				}
				awardBudgetPeriod
						.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
				awardBudgetPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
				awardBudgetPeriod.setTotalCost(
						totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
				awardBudgetPeriod = awardBudgetDao.saveOrUpdateAwardBudgetPeriod(awardBudgetPeriod);
				awardBudgetPeriodList.add(awardBudgetPeriod);
			} else {
				awardBudgetPeriodList.add(awardBudgetPeriod);
			}
		}
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(awardBudgetPeriodList);
		updateBudgetHeader(awardBudgetHeader);
	}

	@Override
	public AwardBudgetDetailCalcAmount getNewAwardBudgetCalculatedAmount(AwardBudgetPeriod budgetPeriod,
			AwardBudgetDetail budgetDetail, AwardRates awardRates) {
		AwardBudgetDetailCalcAmount budgetCalculatedAmount = new AwardBudgetDetailCalcAmount();
		budgetCalculatedAmount.setBudgetId(budgetPeriod.getBudgetId());
		budgetCalculatedAmount.setBudgetPeriod(budgetDetail.getBudgetPeriod());
		budgetCalculatedAmount.setBudgetPeriodId(budgetPeriod.getBudgetPeriodId());
		budgetCalculatedAmount.setLineItemNumber(budgetDetail.getLineItemNumber());
		budgetCalculatedAmount.setRateClassCode(awardRates.getRateClassCode());
		budgetCalculatedAmount.setRateClass(awardRates.getRateClass());
		budgetCalculatedAmount.setRateTypeCode(awardRates.getRateTypeCode());
		budgetCalculatedAmount.setRateType(awardRates.getRateType());
		budgetCalculatedAmount.setApplyRateFlag(true);
		budgetCalculatedAmount.setRateTypeDescription(awardRates.getRateType().getDescription());
		budgetCalculatedAmount.setBudgetDetailId(budgetDetail.getBudgetDetailId());
		budgetCalculatedAmount.setApplicableRate(awardRates.getApplicableRate());
		budgetCalculatedAmount.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		budgetCalculatedAmount.setUpdateUser(budgetDetail.getUpdateUser());
		return awardBudgetDao.saveOrUpdateAwardBudgetDetailCalcAmount(budgetCalculatedAmount);
	}

	private void updateAwardBudgetHeader(AwardBudgetHeader budget) {
		List<AwardBudgetPeriod> budgetPeriods = new ArrayList<>();
		budgetPeriods.addAll(fetchAwardBudgetPeriods(budget));
		BigDecimal totalCost = BigDecimal.ZERO;
		BigDecimal totalDirectCost = BigDecimal.ZERO;
		BigDecimal totalIndirectCost = BigDecimal.ZERO;
		BigDecimal subcontractCost = BigDecimal.ZERO;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (AwardBudgetPeriod period : budgetPeriods) {
				if (period.getTotalCost() != null) {
					totalCost = totalCost.add(period.getTotalCost());
				}
				if (period.getTotalDirectCost() != null) {
					totalDirectCost = totalDirectCost.add(period.getTotalDirectCost());
				}
				if (period.getTotalIndirectCost() != null) {
					totalIndirectCost = totalIndirectCost.add(period.getTotalIndirectCost());
				}
				if (period.getSubcontractCost() != null) {
					subcontractCost = subcontractCost.add(period.getSubcontractCost());
				}
			}
			budget.setTotalDirectCost(totalDirectCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
			budget.setTotalIndirectCost(totalIndirectCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
			budget.setTotalCost(totalCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
		}
	}
	
	@Override
	public String copyAwardBudget(AwardBudgetVO vo) {
		AwardBudgetHeader orginalBudget = null;
		Integer awardId = vo.getAwardId();
		List<AwardBudgetHeader> budgets = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(awardId);
		if (budgets != null && !budgets.isEmpty()) {
			for (AwardBudgetHeader budgetHeader : budgets) {
				if (budgetHeader.getIsLatestVersion()) {
					budgetHeader.setIsLatestVersion(false);
					awardBudgetDao.saveBudgetHeader(budgetHeader);
				}
			}
		}
		if (vo.getAwardBudgetId() != null) {
			orginalBudget = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		}
		AwardBudgetHeader copyBudget = new AwardBudgetHeader();
		Integer budgetVersionNumber = awardBudgetDao.maxAwardBudgetVersionNumberByAwardId(awardId);
		if (budgetVersionNumber == null) {
			budgetVersionNumber = 1;
		} else {
			budgetVersionNumber = budgetVersionNumber + 1;
		}
		copyBudget.setAwardId(orginalBudget.getAwardId());
		createBudgetHeader(copyBudget, orginalBudget, vo.getUserName(), budgetVersionNumber);
		if (orginalBudget.getBudgetPeriods() != null && !orginalBudget.getBudgetPeriods().isEmpty()) {
			copyBudget.getBudgetPeriods()
					.addAll(copyBudgetPeriods(copyBudget, orginalBudget, vo.getActivityTypeCode(), vo.getUserName()));
		}
		updateAwardBudgetHeader(copyBudget);
		awardBudgetDao.saveBudgetHeader(copyBudget);
		vo.setAwardBudgetHeader(copyBudget);
		return commonDao.convertObjectToJSON(vo);
	}

	private void createBudgetHeader(AwardBudgetHeader copyBudget, AwardBudgetHeader originalBudget, String userName,
			Integer budgetVersionNumber) {
		copyBudget.setStartDate(originalBudget.getStartDate());
		copyBudget.setEndDate(originalBudget.getEndDate());
		copyBudget.setCreateTimeStamp(commonDao.getCurrentTimestamp());
		copyBudget.setCreateUser(userName);
		copyBudget.setCreateUserName(userName);
		copyBudget.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		copyBudget.setUpdateUser(userName);
		copyBudget.setUpdateUserName(userName);
		copyBudget.setRateType(originalBudget.getRateType());
		copyBudget.setComments(originalBudget.getComments());
		copyBudget.setRateClassCode(originalBudget.getRateClassCode());
		copyBudget.setRateTypeCode(originalBudget.getRateTypeCode());
		copyBudget.setIsAutoCalc(originalBudget.getIsAutoCalc());
		copyBudget.setAvailableFund(originalBudget.getAvailableFund());
		copyBudget.setVersionNumber(budgetVersionNumber);
		if (originalBudget.getBudgetStatusCode().equals(Constants.BUDGET_STATUS_SUBMITTED)) {
			copyBudget.setBudgetStatusCode(Constants.BUDGET_STATUS_INPROGRESS);
			copyBudget.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(Constants.BUDGET_STATUS_INPROGRESS));
		} else {
			copyBudget.setBudgetStatusCode(originalBudget.getBudgetStatusCode());
			copyBudget.setBudgetStatus(originalBudget.getBudgetStatus());
		}
		copyBudget.setIsLatestVersion(true);
	}

	@Override
	public List<AwardBudgetPeriod> copyBudgetPeriods(AwardBudgetHeader copyBudget, AwardBudgetHeader orginalBudget,
			String activityTypeCode, String userName) {
		List<AwardBudgetPeriod> budgetPeriods = orginalBudget.getBudgetPeriods();
		List<AwardBudgetPeriod> copiedBudgetPeriods = new ArrayList<>(budgetPeriods);
		Collections.copy(copiedBudgetPeriods, budgetPeriods);
		List<AwardBudgetPeriod> newPeriods = new ArrayList<>();
		for (AwardBudgetPeriod originalPeriod : copiedBudgetPeriods) {
			AwardBudgetPeriod copyPeriod = new AwardBudgetPeriod();
			copyPeriod.setAwardNumber(originalPeriod.getAwardNumber());
			copyPeriod.setBudgetId(copyBudget.getBudgetId());
			copyPeriod.setBudgetPeriod(originalPeriod.getBudgetPeriod());
			copyPeriod.setDevProposalId(originalPeriod.getDevProposalId());
			copyPeriod.setDevProposalBudgetPeriod(originalPeriod.getBudgetPeriod());
			copyPeriod.setDevProposalBudgetId(originalPeriod.getDevProposalBudgetId());
			copyPeriod.setEndDate(originalPeriod.getEndDate());
			copyPeriod.setPeriodLabel(originalPeriod.getPeriodLabel());
			copyPeriod.setStartDate(originalPeriod.getStartDate());
			copyPeriod.setSubcontractCost(originalPeriod.getSubcontractCost());
			copyPeriod.setTotalCost(originalPeriod.getTotalCost());
			copyPeriod.setTotalDirectCost(originalPeriod.getTotalDirectCost());
			copyPeriod.setTotalIndirectCost(originalPeriod.getTotalIndirectCost());
			copyPeriod.setUpdateTimeStamp(originalPeriod.getUpdateTimeStamp());
			copyPeriod.setUpdateUser(originalPeriod.getUpdateUser());
			if (originalPeriod.getBudgetDetails() != null && !originalPeriod.getBudgetDetails().isEmpty()) {
				copyAwardBudgetDetails(copyPeriod, originalPeriod, userName);
			}
			newPeriods.add(copyPeriod);
		}
		return newPeriods;
	}

	private void copyAwardBudgetDetails(AwardBudgetPeriod copyPeriod, AwardBudgetPeriod period, String updateUser) {
		List<AwardBudgetDetail> budgetDetails = period.getBudgetDetails();
		List<AwardBudgetPerson> budgetPersons = new ArrayList<>();
		if (budgetDetails != null && !budgetDetails.isEmpty()) {
			List<AwardBudgetDetail> copiedBudgetDetails = new ArrayList<>(budgetDetails);
			Collections.copy(copiedBudgetDetails, budgetDetails);
			List<AwardBudgetDetail> newLineItems = new ArrayList<>();
			for (AwardBudgetDetail budgetDetail : copiedBudgetDetails) {
				AwardBudgetDetail copyBudgetDetail = new AwardBudgetDetail();
				copyBudgetDetail.setBudgetDetailId(null);
				copyBudgetDetail.setAwardNumber(budgetDetail.getAwardNumber());
				copyBudgetDetail.setBalanceToDate(budgetDetail.getBalanceToDate());
				copyBudgetDetail.setBudgetCategory(budgetDetail.getBudgetCategory());
				copyBudgetDetail.setBudgetCategoryCode(budgetDetail.getBudgetCategoryCode());
				copyBudgetDetail.setBudgetJustification(budgetDetail.getBudgetJustification());
				copyBudgetDetail.setBudgetPeriod(copyPeriod.getBudgetPeriod());
				copyBudgetDetail.setCostElement(budgetDetail.getCostElement());
				copyBudgetDetail.setCostElementCode(budgetDetail.getCostElementCode());
				copyBudgetDetail.setCostSharingAmount(budgetDetail.getCostSharingAmount());
				copyBudgetDetail.setCostSharingPercentage(budgetDetail.getCostSharingPercentage());
				copyBudgetDetail.setEndDate(budgetDetail.getEndDate());
				copyBudgetDetail.setInternalOrderCode(budgetDetail.getInternalOrderCode());
				copyBudgetDetail.setIsApplyInflationRate(budgetDetail.getIsApplyInflationRate());
				copyBudgetDetail.setIsSystemGeneratedCostElement(budgetDetail.getIsSystemGeneratedCostElement());
				copyBudgetDetail.setLineItemCost(budgetDetail.getLineItemCost());
				copyBudgetDetail.setLineItemDescription(budgetDetail.getLineItemDescription());
				copyBudgetDetail.setLineItemNumber(budgetDetail.getLineItemNumber());
				copyBudgetDetail.setOnOffCampusFlag(budgetDetail.getOnOffCampusFlag());
				copyBudgetDetail.setBudgetPeriodId(copyPeriod.getBudgetPeriod());
				copyBudgetDetail.setPrevLineItemCost(budgetDetail.getPrevLineItemCost());
				copyBudgetDetail.setQuantity(budgetDetail.getQuantity());
				copyBudgetDetail.setStartDate(budgetDetail.getStartDate());
				copyBudgetDetail.setSystemGeneratedCEType(budgetDetail.getSystemGeneratedCEType());
				copyBudgetDetail.setTbnId(budgetDetail.getTbnId());
				copyBudgetDetail.setTbnPerson(budgetDetail.getTbnPerson());
				copyBudgetDetail.setUpdateTimeStamp(budgetDetail.getUpdateTimeStamp());
				copyBudgetDetail.setUpdateUser(updateUser);
				copyBudgetDetail.setVersionNumber(budgetDetail.getVersionNumber());
				List<AwardBudgetPersonalDetail> awardBudgetPersonalDetails = budgetDetail.getPersonsDetails();
				if (awardBudgetPersonalDetails != null && !awardBudgetPersonalDetails.isEmpty()) {
					List<AwardBudgetPerson> awardBudgetPersons = new ArrayList<>();
					awardBudgetPersons = copyBudgetPersons(awardBudgetPersonalDetails, copyPeriod);
					budgetPersons.addAll(setAwardBudgetPersonList(awardBudgetPersons));
					copyBudgetDetail.getPersonsDetails().addAll(setAwardBudgetPersonDetailList(budgetPersons,
							copyBudgetDetail, budgetDetail, updateUser));
				}
				newLineItems.add(copyBudgetDetail);
			}
			copyPeriod.getBudgetDetails().addAll(newLineItems);
		}
	}

	private List<AwardBudgetPerson> setAwardBudgetPersonList(List<AwardBudgetPerson> personList) {
		boolean checkBudgetPersonInBudget = false;
		List<AwardBudgetPerson> budgetPersonsList = new ArrayList<>();
		AwardBudgetPerson budgetPerson = new AwardBudgetPerson();
		for (AwardBudgetPerson person : personList) {
			checkBudgetPersonInBudget = awardBudgetDao.checkBudgetPersonInBudget(person.getBudgetHeaderId(),
					person.getTbnId(), person.getJobCode(), person.getPersonId(), person.getRolodexId());
			if (checkBudgetPersonInBudget) {
				budgetPerson = awardBudgetDao.saveOrUpdateAwardBudgetPerson(person);
			} else {
				if (!person.getTbnId().equals(null)) {
					budgetPerson = awardBudgetDao.getBugetTbnPersonByTbnId(person.getBudgetHeaderId(), person.getTbnId());
				} else if (!person.getBudgetPersonId().equals(null)) {
					budgetPerson = awardBudgetDao.getBugetPersonByPersonId(person.getBudgetHeaderId(), person.getPersonId());
				} else {
					budgetPerson = awardBudgetDao.getBugetRolodexPersonByRolodexId(person.getBudgetHeaderId(),
							person.getRolodexId());
				}
			}
			budgetPersonsList.add(budgetPerson);
		}
		return budgetPersonsList;
	}

	private List<AwardBudgetPersonalDetail> setAwardBudgetPersonDetailList(List<AwardBudgetPerson> budgetPersonsList,
			AwardBudgetDetail copyBudgetDetail, AwardBudgetDetail budgetDetail, String updateUser) {
		List<AwardBudgetPersonalDetail> budgetPersonDetails = new ArrayList<>();
		List<AwardBudgetPersonalDetail> budgetPersonDetailsList = new ArrayList<>();
		budgetPersonDetails.addAll(copyBudgetPersonalDetails(copyBudgetDetail, budgetDetail, updateUser));
		for (AwardBudgetPerson budgetPersons : budgetPersonsList) {
			for (AwardBudgetPersonalDetail budgetPersonalDetails : budgetPersonDetails) {
				if (budgetPersons.getPersonType().equals(budgetPersonalDetails.getBudgetPerson().getPersonType())) {
					if (budgetPersonalDetails.getBudgetPerson().getPersonType().equals("T")) {
						if (budgetPersons.getTbnId().equals(budgetPersonalDetails.getBudgetPerson().getTbnId())) {
							budgetPersonalDetails.setBudgetPersonId(budgetPersons.getBudgetPersonId());
							budgetPersonalDetails.setBudgetPerson(budgetPersons);
						}
					} else if (budgetPersonalDetails.getBudgetPerson().getPersonType().equals("E")) {
						if (budgetPersons.getPersonId().equals(budgetPersonalDetails.getBudgetPerson().getPersonId())) {
							budgetPersonalDetails.setBudgetPersonId(budgetPersons.getBudgetPersonId());
							budgetPersonalDetails.setBudgetPerson(budgetPersons);
						}
					} else {
						if (budgetPersons.getRolodexId()
								.equals(budgetPersonalDetails.getBudgetPerson().getRolodexId())) {
							budgetPersonalDetails.setBudgetPersonId(budgetPersons.getBudgetPersonId());
							budgetPersonalDetails.setBudgetPerson(budgetPersons);
						}
					}
					if (budgetPersonDetailsList.isEmpty()) {
						budgetPersonDetailsList.add(budgetPersonalDetails);
					} else {
						if (!budgetPersonDetailsList.contains(budgetPersonalDetails)) {
							budgetPersonDetailsList.add(budgetPersonalDetails);
						}
					}
				}
			}
		}
		return budgetPersonDetailsList;
	}

	private List<AwardBudgetPerson> copyBudgetPersons(List<AwardBudgetPersonalDetail> copiedBudgetPersonDetailList,
			AwardBudgetPeriod copyPeriod) {
		List<AwardBudgetPerson> awardBudgetPersonList = new ArrayList<>();
		for (AwardBudgetPersonalDetail copiedBudgetPersonDetail : copiedBudgetPersonDetailList) {
			AwardBudgetPerson budgetPerson = new AwardBudgetPerson();
			AwardBudgetPerson copiedBudgetPerson = copiedBudgetPersonDetail.getBudgetPerson();
			budgetPerson.setAppointmentType(copiedBudgetPerson.getAppointmentType());
			budgetPerson.setAppointmentTypeCode(copiedBudgetPerson.getAppointmentTypeCode());
			budgetPerson.setBudgetHeaderId(copyPeriod.getBudgetId());
			budgetPerson.setBudgetPersonId(null);
			budgetPerson.setCalculationBase(copiedBudgetPerson.getCalculationBase());
			budgetPerson.setEffectiveDate(copiedBudgetPerson.getEffectiveDate());
			budgetPerson.setJobCode(copiedBudgetPerson.getJobCode());
			budgetPerson.setJobCodes(copiedBudgetPerson.getJobCodes());
			budgetPerson.setPersonId(copiedBudgetPerson.getPersonId());
			budgetPerson.setPersonName(copiedBudgetPerson.getPersonName());
			budgetPerson.setRolodexId(copiedBudgetPerson.getRolodexId());
			budgetPerson.setSalaryAnniversaryDate(copiedBudgetPerson.getSalaryAnniversaryDate());
			budgetPerson.setTbnId(copiedBudgetPerson.getTbnId());
			budgetPerson.setTbnPerson(copiedBudgetPerson.getTbnPerson());
			budgetPerson.setUpdateTimeStamp(copiedBudgetPerson.getUpdateTimeStamp());
			budgetPerson.setUpdateUser(copiedBudgetPerson.getUpdateUser());
			awardBudgetPersonList.add(budgetPerson);
		}
		return awardBudgetPersonList;
	}

	public List<AwardBudgetPersonalDetail> copyBudgetPersonDetails(List<AwardBudgetPersonalDetail> personDetails,
			AwardBudgetDetail budgetdetail, AwardBudgetPeriod currentPeriod) {
		List<AwardBudgetPersonalDetail> copiedBudgetPersons = new ArrayList<>();
		for (AwardBudgetPersonalDetail personDetail : personDetails) {
			AwardBudgetPersonalDetail copiedBudgetPerson = new AwardBudgetPersonalDetail();
			copiedBudgetPerson.setBudgetDetail(budgetdetail);
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
			copiedBudgetPerson.setSalaryRequested(personDetail.getSalaryRequested());
			copiedBudgetPerson.setInternalOrderCode(personDetail.getInternalOrderCode());
			copiedBudgetPerson
					.setBudgetPerson(awardBudgetDao.getAwardBudgetPersonBypersonId(personDetail.getBudgetPersonId()));
			copiedBudgetPersons.add(copiedBudgetPerson);
		}
		return copiedBudgetPersons;
	}

	private List<AwardBudgetPersonalDetail> copyBudgetPersonalDetails(AwardBudgetDetail copyBudgetDetail,
			AwardBudgetDetail budgetDetail, String updateUser) {
		List<AwardBudgetPersonalDetail> budgetPersonDetails = budgetDetail.getPersonsDetails();
		List<AwardBudgetPersonalDetail> copiedBudgetPersonDetails = new ArrayList<>(budgetPersonDetails);
		Collections.copy(copiedBudgetPersonDetails, budgetPersonDetails);
		List<AwardBudgetPersonalDetail> newBudgetPersonalDetails = new ArrayList<>();
		for (AwardBudgetPersonalDetail copiedBudgetPersonDetail : copiedBudgetPersonDetails) {
			AwardBudgetPersonalDetail budgetPersonDetail = new AwardBudgetPersonalDetail();
			budgetPersonDetail.setBudgetPerson(copiedBudgetPersonDetail.getBudgetPerson());
			budgetPersonDetail.setBudgetDetail(copyBudgetDetail);
			budgetPersonDetail.setCostSharingAmount(copiedBudgetPersonDetail.getCostSharingAmount());
			budgetPersonDetail.setCostSharingPercentage(copiedBudgetPersonDetail.getCostSharingPercentage());
			budgetPersonDetail.setNoOfMonths(copiedBudgetPersonDetail.getNoOfMonths());
			budgetPersonDetail.setPercentageCharged(copiedBudgetPersonDetail.getPercentageCharged());
			budgetPersonDetail.setPercentageEffort(copiedBudgetPersonDetail.getPercentageEffort());
			budgetPersonDetail.setSalaryRequested(copiedBudgetPersonDetail.getSalaryRequested());
			budgetPersonDetail.setTotalSalary(copiedBudgetPersonDetail.getTotalSalary());
			budgetPersonDetail.setUnderRecoveryAmount(copiedBudgetPersonDetail.getUnderRecoveryAmount());
			budgetPersonDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			budgetPersonDetail.setUpdateUser(updateUser);
			newBudgetPersonalDetails.add(budgetPersonDetail);
		}
		return newBudgetPersonalDetails;
	}

	@Override
	public String saveOrUpdateAwardBudgetOverView(AwardBudgetVO vo) {
		AwardBudgetHeader budgetHeader = vo.getAwardBudgetHeader();
		fetchAwardBudgetPeriods(budgetHeader);
		Award award = awardDao.fetchAwardByAwardId(budgetHeader.getAwardId().toString());
		award.setAccountNumber(budgetHeader.getFundCode());
		award.setFundCenter(budgetHeader.getFundCenter());
		award.setDocumentUpdateUser(budgetHeader.getUpdateUser());
		award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardDao.saveOrUpdateAwardDetails(award);
		vo.setAward(award);
		vo.setIsCreated(true);
		vo.setAwardBudgetHeader(budgetHeader);
		if (budgetHeader.getVersionNumber() > 1) {
			setAwardBudgetHeaderBasedOnVersionNumber(budgetHeader, vo);
		}
		budgetHeader = awardBudgetDao.saveOrUpdateAwardBudgetOverView(vo.getAwardBudgetHeader());
		if (vo.getBudgetTemplateTypeId() != null) {
			String defaultAbPersonItemToBeTBN = commonDao.getParameterValueAsString(Constants.DEFAULT_AB_TBN_PERSON_ID);
			addTemplateCostElementForAward(budgetHeader, defaultAbPersonItemToBeTBN, award, vo.getBudgetTemplateTypeId());
		}
		return commonDao.convertObjectToJSON(calculateAwardBudgetOverView(budgetHeader.getBudgetId()));
	}

	@Override
	public String saveOrUpdateAwardBudgetPeriod(List<AwardBudgetPeriod> awardBudgetPeriod, Integer budgetId, String updateUser) {
		List<AwardBudgetPeriod> awardBudgetPeriodList = new ArrayList<>();
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
		for (AwardBudgetPeriod budgetPeriod : awardBudgetPeriod) {
			budgetPeriod.setBudgetId(awardBudgetHeader.getBudgetId());
			budgetPeriod.setAwardNumber(awardBudgetHeader.getAwardNumber());
			budgetPeriod = awardBudgetDao.saveOrUpdateAwardBudgetPeriod(budgetPeriod);
			awardBudgetPeriodList.add(budgetPeriod);
		}
		awardBudgetHeader.getBudgetPeriods().addAll(awardBudgetPeriodList);
		updateBudgetHeader(awardBudgetHeader);
		fetchAwardBudgetPeriods(awardBudgetHeader);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardBudgetHeader.getAwardId(), updateUser);
		return commonDao.convertObjectToJSON(awardBudgetHeader);
	}

	@Override
	public String saveOrUpdateAwardBudgetLineItem(AwardDetailBudgetVO vo) {
		List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
		AwardBudgetDetail awardBudgetDetail = vo.getAwardBudgetDetail();
		awardBudgetDetail.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardBudgetDetail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		Award award = awardDao.fetchAwardByAwardId(vo.getAwardId());
		Integer lineItemNumber = 1;
		if (vo.getLineItemCount() < fetchSysGeneratedCESize(award.getActivityTypeCode())) {
			List<CostElement> sysGeneratedCostElements = fetchSysGeneratedCostElements(award.getActivityTypeCode());
			for (CostElement costElement : sysGeneratedCostElements) {
				budgetDetails.add(setAwardBudgetDetailData(costElement, awardBudgetDetail, lineItemNumber));
				lineItemNumber++;
			}
		}
		if (awardBudgetDetail.getBudgetDetailId() == null) {
			Integer itemNumber = awardBudgetDao.getMaxLineItemNumberByPeriodId(awardBudgetDetail.getBudgetPeriodId());
			if (itemNumber == null) {
				lineItemNumber = 1;
			} else {
				lineItemNumber = awardBudgetDao.getMaxLineItemNumberByPeriodId(awardBudgetDetail.getBudgetPeriodId())
						+ lineItemNumber;
			}
			awardBudgetDetail.setLineItemNumber(lineItemNumber);
		}
		if (Boolean.TRUE.equals(vo.getCostElementChanged())) {
			if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION) 
					&& award.getAccountNumber() != null) {
				String previousInternalCode = awardBudgetDetail.getInternalOrderCode();
				awardBudgetDetail.setInternalOrderCode(updateInternalOrderCode(awardBudgetDetail.getBudgetId(), awardBudgetDetail.getBudgetCategoryCode(), award.getAccountNumber()));
				String budgetReferenceNumber = previousInternalCode != null ? previousInternalCode : null;
				deleteManpower(budgetReferenceNumber, vo.getAwardId());
			} else if(award.getAccountNumber() == null) {
				String budgetReferenceNumber = awardBudgetDetail.getBudgetDetailId().toString();
				deleteManpower(budgetReferenceNumber, vo.getAwardId());
			}
		}
		budgetDetails.add(awardBudgetDetail);
		boolean isEnableIOCodeGeneration = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_IO_CODE_GENERATION);
		boolean isEnable = (!Constants.AWARD_SETUP.equals(award.getAwardDocumentTypeCode()) || Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED.equals(award.getAwardWorkflowStatus().getWorkflowAwardStatusCode()));
		for (AwardBudgetDetail budgetItemDetail : budgetDetails) {
			budgetItemDetail.setAwardNumber(award.getAwardNumber());
			generateLineItemIOCode(award,budgetItemDetail, isEnableIOCodeGeneration,isEnable);
			budgetItemDetail = awardBudgetDao.saveOrUpdateAwardBudgetLineItem(budgetItemDetail);
		}
		AwardBudgetVO awardBudgetVO = calculateAwardBudget(awardBudgetDetail.getBudgetId());
		AwardBudgetHeader awardBudgetHeader = awardBudgetVO.getAwardBudgetHeader();
		if (awardBudgetHeader.getVersionNumber() > 1) {
			setAwardBudgetHeaderBasedOnVersionNumber(awardBudgetHeader, awardBudgetVO);
		}
		awardService.updateAwardDocumentUpdateUserAndTimestamp(Integer.parseInt(vo.getAwardId()), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(awardBudgetVO);
	}

	private void deleteManpower(String budgetReferenceNumber, String awardId) {
		if (Boolean.TRUE.equals(commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED))) {
			AwardManpower awardManpower = manpowerDao.fetchAwardManpowerDetails(budgetReferenceNumber, Integer.parseInt(awardId));
			if (awardManpower != null) {
				List<AwardManpowerResource> awardManpowerResources = manpowerDao.getAwardManpowerResourcesByManpowerId(awardManpower.getAwardManpowerId());
				if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
					manpowerDao.deleteAllManpowerResource(awardManpowerResources);
				}
				manpowerDao.deleteAwardManpower(awardManpower.getAwardManpowerId());
			}
		}
	}

	private void generateLineItemIOCode(Award award,AwardBudgetDetail budgetItemDetail, boolean isEnableIOCodeGeneration, boolean isEnable) {
		if (budgetItemDetail.getBudgetDetailId() == null && isEnableIOCodeGeneration && budgetItemDetail.getInternalOrderCode() == null && isEnable){
			String ioCode = wbsDao.generateIOCode(award.getAwardId(), null ,"NEW", budgetItemDetail.getCostElementCode(), budgetItemDetail.getBudgetPeriodId());
			budgetItemDetail.setInternalOrderCode(ioCode);
		}
	}
	private AwardBudgetDetail setAwardBudgetDetailData(CostElement costElement, AwardBudgetDetail awardBudgetDetail,
			Integer lineItemNumber) {
		AwardBudgetDetail detail = new AwardBudgetDetail();
		detail.setAwardNumber(awardBudgetDetail.getAwardNumber());
		detail.setBudgetCategory(costElement.getBudgetCategory());
		detail.setBudgetCategoryCode(costElement.getBudgetCategoryCode());
		detail.setBudgetId(awardBudgetDetail.getBudgetId());
		detail.setBudgetJustification(awardBudgetDetail.getBudgetJustification());
		detail.setBudgetPeriod(awardBudgetDetail.getBudgetPeriod());
		detail.setBudgetPeriodId(awardBudgetDetail.getBudgetPeriodId());
		detail.setCostElement(costElement);
		detail.setCostElementCode(costElement.getCostElement());
		detail.setIsApplyInflationRate(awardBudgetDetail.getIsApplyInflationRate());
		detail.setIsSystemGeneratedCostElement(true);
		detail.setLineItemCost(BigDecimal.ZERO);
		detail.setLineItemNumber(lineItemNumber);
		detail.setOnOffCampusFlag(awardBudgetDetail.getOnOffCampusFlag());
		detail.setSystemGeneratedCEType(costElement.getSystemGeneratedCEType());
		detail.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		detail.setUpdateUser(awardBudgetDetail.getUpdateUser());
		detail.setVersionNumber(awardBudgetDetail.getVersionNumber());
		return detail;
	}

	private String updateInternalOrderCode(Integer budgetId, String budgetCategoryCode, String accountNumber) {
		StringBuilder internalOrderCode = new StringBuilder();
		String nextLineItemSequenceNumber = StringUtils.leftPad(awardBudgetDao.getNextLineItemSequenceNumber(budgetId, budgetCategoryCode, accountNumber), 2, "0");
		return internalOrderCode.append(accountNumber).append(budgetCategoryCode).append(nextLineItemSequenceNumber).toString(); 
	}

	@Override
	public AwardBudgetVO calculateAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader) {
		AwardBudgetVO vo = new AwardBudgetVO();
		Award award = awardDao.getAwardDetailsById(awardBudgetHeader.getAwardId());
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(awardBudgetHeader.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
		}
		awardBudgetHeader.setUpdateUserName(personDao.getUserFullNameByUserName(awardBudgetHeader.getUpdateUser()));
		awardBudgetHeader.setCreateUserName(personDao.getUserFullNameByUserName(awardBudgetHeader.getCreateUser()));
		awardBudgetHeader = updateBudgetHeader(awardBudgetHeader);
		setAwardBudgetRates(vo, awardBudgetHeader);
		vo.setAwardBudgetHeader(awardBudgetHeader);
		vo.setAwardBudgetList(budgetHeaderDetails);
		vo.setAward(award);
		vo.setIsCreated(true);
		vo.setUserName(awardBudgetHeader.getUpdateUser());
		vo.setUserFullName(personDao.getUserFullNameByUserName(vo.getUserName()));
		vo.setEnableAbPersonAppliedSalary(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_PERSON_APPL_SAL_CALC));
		return vo;
	}

	public AwardBudgetVO calculateAwardBudget(Integer BudgetId) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(BudgetId);
		List<AwardRates> awardRates = awardBudgetDao.fetchAwardRatesByBudgetId(BudgetId);
		if (awardRates != null && !awardRates.isEmpty()) {
			awardBudgetHeader.setAwardRates(awardRates);
		}
		if (awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			fetchAwardBudgetPeriods(awardBudgetHeader);
		}
		calculateAwardBudgetPeriod(awardBudgetHeader);
		return calculateAwardBudgetHeader(awardBudgetHeader);
	}

	public AwardBudgetVO calculateAwardBudgetOverView(Integer BudgetId) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(BudgetId);
		List<AwardRates> awardRates = awardBudgetDao.fetchAwardRatesByBudgetId(BudgetId);
		if (awardRates != null && !awardRates.isEmpty()) {
			awardBudgetHeader.setAwardRates(awardRates);
		}
		if (awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			fetchAwardBudgetPeriods(awardBudgetHeader);
		}
		calculateAwardBudgetPeriodOnImport(awardBudgetHeader);
		return calculateAwardBudgetHeader(awardBudgetHeader);
	}

	
	public AwardBudgetVO calculateCopyAwardBudget(AwardBudgetVO vo, List<AwardBudgetDetail> newAwardBudgetDetails) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		calculateCopyAwardBudgets(vo, awardBudgetHeader, newAwardBudgetDetails);
		return calculateAwardBudgetHeader(awardBudgetHeader);
	}

	private void calculateCopyAwardBudgets(AwardBudgetVO vo, AwardBudgetHeader awardBudgetHeader,
			List<AwardBudgetDetail> newAwardBudgetDetails) {
		AwardBudgetPeriod currentPeriod = awardBudgetDao.getPeriodById(vo.getCurrentPeriodId());
		BigDecimal totalFringeCost = BigDecimal.ZERO;
		BigDecimal totalFandACost = BigDecimal.ZERO;
		BigDecimal totalLineItemCost = BigDecimal.ZERO;
		List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
		budgetDetails.addAll(newAwardBudgetDetails);
		if (!budgetDetails.isEmpty() && budgetDetails != null) {
			for (AwardBudgetDetail awardBudgetDetail : budgetDetails) {
				if (awardBudgetDetail.getIsSystemGeneratedCostElement()) {
					if (Constants.BUDGET_FRINGE_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
							|| Constants.BUDGET_FRINGE_OFF.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
						totalFringeCost = totalFringeCost.add(awardBudgetDetail.getLineItemCost());
					}
					if (Constants.BUDGET_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
							|| Constants.BUDGET_OH_OFF.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
						totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
					}
					if (Constants.BUDGET_RESEARCH_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
						totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
					}
				} else {
					if (awardBudgetDetail.getLineItemCost() != null) {
						totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
					}
				}
			}
			currentPeriod.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
			currentPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
			currentPeriod.setTotalCost(
					totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
		}
		currentPeriod = awardBudgetDao.saveOrUpdateAwardBudgetPeriod(currentPeriod);
		if(awardBudgetHeader.getIsAutoCalc()) {
			Award award = awardDao.fetchAwardByAwardId(awardBudgetHeader.getAwardId().toString());
			if (awardBudgetHeader.getAwardRates().isEmpty()) {
				Set<String> rateClassTypes = new HashSet<>();
				List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(awardBudgetHeader, award.getActivityTypeCode(),
						rateClassTypes);
				saveOrUpdateAwardBudgetRates(awardBudgetRates);
				awardBudgetHeader.getAwardRates().addAll(awardBudgetRates);
			}
			calculate(awardBudgetHeader, null, award.getActivityTypeCode());
		}
		updateBudgetHeader(awardBudgetHeader);
	}

	@Override
	public void calculateAwardBudgetPeriod(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
			BigDecimal totalFringeCost = BigDecimal.ZERO;
			BigDecimal totalFandACost = BigDecimal.ZERO;
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			List<AwardBudgetDetail> budgetDetail = awardBudgetPeriod.getBudgetDetails();
			if (!budgetDetail.isEmpty() && budgetDetail != null) {
				for (AwardBudgetDetail awardBudgetDetail : budgetDetail) {
					awardBudgetDetail.setBudgetDetailCalcAmounts(awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(awardBudgetDetail.getBudgetDetailId()));
					if (awardBudgetPeriod.getBudgetPeriodId().equals(awardBudgetDetail.getBudgetPeriodId())) {
						if (Boolean.TRUE.equals(awardBudgetDetail.getIsSystemGeneratedCostElement())) {
							if (Constants.BUDGET_FRINGE_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
									|| Constants.BUDGET_FRINGE_OFF
											.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFringeCost = totalFringeCost.add(awardBudgetDetail.getLineItemCost());
							}
							if (Constants.BUDGET_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
									|| Constants.BUDGET_OH_OFF.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
							}
							if (Constants.BUDGET_RESEARCH_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
							}
						} else {
							if (awardBudgetDetail.getLineItemCost() != null) {
								totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
							}
						}
					}
					if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_WBS_GENERATION) && awardDao.getAccountNumberByAwardId(awardBudgetHeader.getAwardId()) != null && awardBudgetDetail.getInternalOrderCode() == null) {
						wbsDao.generateWBSNumber(awardBudgetHeader.getAwardId(), "N", awardBudgetDetail.getBudgetDetailId(), awardBudgetDetail.getBudgetCategoryCode());
					}
					awardBudgetDetail.setInternalOrderCode(awardBudgetDao.getInternalOrderCodeByBudgetDetailId(awardBudgetDetail.getBudgetDetailId()));
				}
			}
			awardBudgetPeriod
					.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
			awardBudgetPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
			awardBudgetPeriod.setTotalCost(
					totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
			awardBudgetPeriod = awardBudgetDao.saveOrUpdateAwardBudgetPeriod(awardBudgetPeriod);
			awardBudgetPeriods.add(awardBudgetPeriod);
		}
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(awardBudgetPeriods);
		if (Boolean.TRUE.equals(awardBudgetHeader.getIsAutoCalc())) {
			String awardActivityTypeCode = awardDao.getAwardActivityTypeCodeByAwardId(awardBudgetHeader.getAwardId());
			List<AwardRates> rates = awardBudgetHeader.getAwardRates();
			if (rates.isEmpty()) {
				Set<String> rateClassTypes = new HashSet<>();
				List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(awardBudgetHeader, awardActivityTypeCode, rateClassTypes);
				saveOrUpdateAwardBudgetRates(awardBudgetRates);
				awardBudgetHeader.getAwardRates().addAll(awardBudgetRates);
			}
			calculate(awardBudgetHeader, null, awardActivityTypeCode);
		}
		updateBudgetHeader(awardBudgetHeader);
	}
	
	private void calculateAwardBudgetPeriodOnImport(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
			BigDecimal totalFringeCost = BigDecimal.ZERO;
			BigDecimal totalFandACost = BigDecimal.ZERO;
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			List<AwardBudgetDetail> budgetDetail = awardBudgetPeriod.getBudgetDetails();
			if (!budgetDetail.isEmpty() && budgetDetail != null) {
				for (AwardBudgetDetail awardBudgetDetail : budgetDetail) {
					if (awardBudgetPeriod.getBudgetPeriodId().equals(awardBudgetDetail.getBudgetPeriodId())) {
						if (awardBudgetDetail.getIsSystemGeneratedCostElement()) {
							if (Constants.BUDGET_FRINGE_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
									|| Constants.BUDGET_FRINGE_OFF
											.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFringeCost = totalFringeCost.add(awardBudgetDetail.getLineItemCost());
							}
							if (Constants.BUDGET_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())
									|| Constants.BUDGET_OH_OFF.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
							}
							if (Constants.BUDGET_RESEARCH_OH_ON.equals(awardBudgetDetail.getSystemGeneratedCEType())) {
								totalFandACost = totalFandACost.add(awardBudgetDetail.getLineItemCost());
							}
						} else {
							if (awardBudgetDetail.getLineItemCost() != null) {
								totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
							}
						}
					}
				}
				awardBudgetPeriod
				.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
		awardBudgetPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
		awardBudgetPeriod.setTotalCost(
				totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
			}
			awardBudgetPeriod = awardBudgetDao.saveOrUpdateAwardBudgetPeriod(awardBudgetPeriod);
			awardBudgetPeriods.add(awardBudgetPeriod);
		}
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(awardBudgetPeriods);
		if(awardBudgetHeader.getIsAutoCalc()) {
			Award award = awardDao.fetchAwardByAwardId(awardBudgetHeader.getAwardId().toString());
			if (awardBudgetHeader.getAwardRates().isEmpty()) {
				Set<String> rateClassTypes = new HashSet<>();
				List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(awardBudgetHeader, award.getActivityTypeCode(),
						rateClassTypes);
				saveOrUpdateAwardBudgetRates(awardBudgetRates);
				awardBudgetHeader.getAwardRates().addAll(awardBudgetRates);
			}
			calculate(awardBudgetHeader, null, award.getActivityTypeCode());
		}
		updateBudgetHeader(awardBudgetHeader);
	}
	
	private AwardBudgetHeader updateBudgetHeader(AwardBudgetHeader budget) {
		boolean enableAwardBudgetVirementCalculation = commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION);
		Integer previousVersion = budget.getVersionNumber() - 1;
		BigDecimal previousCumilativeVirement = BigDecimal.ZERO;
		BigDecimal previousAvailableFunds = BigDecimal.ZERO;
		BigDecimal virement = BigDecimal.ZERO;
		BigDecimal cumulativeVirement;
		BigDecimal availableFund;
		BigDecimal previousTotalCost = BigDecimal.ZERO;
		List<AwardBudgetPeriod> budgetPeriods = budget.getBudgetPeriods();
		List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(budget.getAwardNumber());
		for (AwardBudgetHeader awardBudgetHeader : awardBudgetHeaders) {
			if (awardBudgetHeader.getVersionNumber().equals(previousVersion) && (!awardBudgetHeader.getIsLatestVersion().equals(true)) ) {
				previousCumilativeVirement = awardBudgetHeader.getCumulativeVirement();
				previousAvailableFunds = calculateAvailableFund(awardBudgetHeader.getAwardId(), awardBudgetHeader.getAwardNumber(), true, awardBudgetHeader.getAvailableFundType());
				previousTotalCost = awardBudgetHeader.getTotalCost();
			}
		}
		Award award = awardDao.fetchAwardByAwardId(budget.getAwardId().toString());
		BigDecimal totalDirectCost = BigDecimal.ZERO;
		BigDecimal totalIndirectCost = BigDecimal.ZERO;
		BigDecimal totalCost = BigDecimal.ZERO;
		BigDecimal totalSubcontractCost = BigDecimal.ZERO;
		BigDecimal lineItemCostChange = BigDecimal.ZERO;
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (AwardBudgetPeriod period : budgetPeriods) {
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
				lineItemCostChange = lineItemCostChange.add(calculateVirement(period.getBudgetPeriodId()));
			}
		}
		availableFund = calculateAvailableFund(budget.getAwardId(), budget.getAwardNumber(), true, budget.getAvailableFundType());
		if (enableAwardBudgetVirementCalculation) {
			if (previousAvailableFunds.equals(availableFund) && (previousTotalCost.equals(budget.getTotalCost()))) {
				virement = virement.add(calculateAwardBudgetVirment(budget.getBudgetId(), lineItemCostChange));
			} else {
				virement = BigDecimal.ZERO;
			}
			if (budget.getSequenceNumber() <= 2) {
				cumulativeVirement = calculateCumilativeVirement(virement);
			} else {
				if (virement.equals(BigDecimal.ZERO)) {
					cumulativeVirement = previousCumilativeVirement;
				} else {
					cumulativeVirement =calculateCumilativeVirement(virement).add(previousCumilativeVirement);
				}
			}
			budget.setVirement(virement);
			budget.setCumulativeVirement(cumulativeVirement);
		} else {
			budget.setVirement(BigDecimal.ZERO);
			budget.setCumulativeVirement(BigDecimal.ZERO);
		}
		budget.setTotalDirectCost(totalDirectCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
		budget.setTotalIndirectCost(totalIndirectCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
		budget.setTotalCost(totalCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
		budget.setFundCode(award.getAccountNumber());
		budget.setFundCenter(award.getFundCenter());
		budget.setInitialAvailableFund(availableFund);
		budget.setAvailableFund(budget.getInitialAvailableFund().subtract(budget.getTotalCost()));
		budget.setEnableAwardBudgetVirementCalculation(enableAwardBudgetVirementCalculation);
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		budget.setManpowerEnabled(manpowerEnabled);
		budget.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
		budget.setObligatedTotal(calculateObligatedTotal(budget.getAwardId(), budget.getAwardNumber()));
		budget.setAnticipatedTotal(calculateAnticipatedAmount(budget.getAwardId(), budget.getAwardNumber()));
		budget.setTotalCostShare(calculateCostShareIncludedInBudget(budget.getAwardId()));
		return awardBudgetDao.saveOrUpdateAwardBudgetOverView(budget);
	}

	@Override
	public String deleteAwardBudgetPeriod(Integer budgetPeriodId, String updateUser) {
		AwardBudgetPeriod awardBudgetPeriod = awardBudgetDao.getPeriodById(budgetPeriodId);
		List<AwardBudgetDetail> awardBudgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(budgetPeriodId);
		Integer budgetId = awardBudgetPeriod.getBudgetId();
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
		int budgetPeriodNumber = 0;
		deleteAwardBudgetDetails(awardBudgetDetails);
		awardBudgetDao.deleteBudgetPeriod(awardBudgetPeriod);
		fetchAwardBudgetPeriods(awardBudgetHeader);
		awardBudgetHeader.getBudgetPeriods().remove(awardBudgetPeriod);
		budgetPeriodNumber = awardBudgetPeriod.getBudgetPeriod();
		if (budgetPeriodNumber > 0) {
			updateBudgetPeriods(awardBudgetHeader.getBudgetPeriods(), budgetPeriodNumber, true);
		}
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardBudgetHeader.getAwardId(), updateUser);
		return commonDao.convertObjectToJSON(calculateAwardBudgetHeader(awardBudgetHeader));
	}

	private void deleteAwardBudgetDetails(List<AwardBudgetDetail> budgetDetails) {
		for (AwardBudgetDetail detail : budgetDetails) {
			awardBudgetDao.deleteBudgetDetail(detail);
		}
	}

	@Override
	public List<AwardBudgetPeriod> getAllBudgetPeriodByBudgetId(Integer budgetId) {
		return awardBudgetDao.getAwardBudgetPeriodsByBudgetId(budgetId);
	}

	@Override
	public String deleteBudgetLineItem(AwardBudgetVO vo) {
		List<AwardBudgetDetail> awardBudgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(vo.getBudgetPeriodId());
		List<AwardBudgetDetail> updatedAwardBudgetDetails = new ArrayList<>(awardBudgetDetails);
		Collections.copy(updatedAwardBudgetDetails, awardBudgetDetails);
		Award award = awardDao.getAwardDetailsById(vo.getAwardId());
		Integer sysGeneratedCESize = fetchSysGeneratedCESize(award.getActivityTypeCode());
		if (Boolean.FALSE.equals(commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED))) {
			deleteAwardBudgetDetail(awardBudgetDetails, updatedAwardBudgetDetails, vo.getBudgetDetailId(), sysGeneratedCESize);
		} else {
			String budgetReferenceNumber = vo.getInternalOrderCode() != null ? vo.getInternalOrderCode() : vo.getBudgetDetailId().toString();
			AwardManpower awardManpower = manpowerDao.fetchAwardManpowerDetails(budgetReferenceNumber, vo.getAwardId());
			if (awardManpower != null) {
				Boolean isAwardManpowerResourceExist = manpowerDao.checkIfAwardManpowerResourceIsExistBasedOnParams(awardManpower.getAwardManpowerId());
				if (Boolean.FALSE.equals(isAwardManpowerResourceExist)) {
					deleteAwardBudgetDetail(awardBudgetDetails, updatedAwardBudgetDetails, vo.getBudgetDetailId(), sysGeneratedCESize);
					manpowerDao.deleteAwardManpower(awardManpower.getAwardManpowerId());
				}
			} else {
				deleteAwardBudgetDetail(awardBudgetDetails, updatedAwardBudgetDetails, vo.getBudgetDetailId(), sysGeneratedCESize);
			}
		}
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
		calculateAwardBudget(vo.getAwardBudgetId());
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(vo.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			if (budget.getBudgetId().equals(awardBudgetHeader.getBudgetId())) {
				budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(awardBudgetHeader));
			} else {
				budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
			}
		}
		vo.setAwardBudgetHeader(awardBudgetHeader);
		vo.setAwardBudgetList(budgetHeaderDetails);
		vo.setAward(award);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardId(), vo.getUserName());
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteAwardBudgetDetail(List<AwardBudgetDetail> awardBudgetDetails, List<AwardBudgetDetail> updatedAwardBudgetDetails, Integer budgetDetailId, Integer sysGeneratedCESize) {
		boolean isSystemGeneratedCostEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_SYS_GENERATED_COST_ELEMENT);
			awardBudgetDetails.stream().forEach(awardBudgetDetail -> {
			if (awardBudgetDetail.getBudgetDetailId().equals(budgetDetailId)) {
				List<AwardBudgetPersonalDetail> awardBudgetPersonalDetails = awardBudgetDetail.getPersonsDetails();
				if (awardBudgetPersonalDetails != null && !awardBudgetPersonalDetails.isEmpty()) {
					for (AwardBudgetPersonalDetail awardBudgetPerson : awardBudgetPersonalDetails) {
						awardBudgetDao.deleteAwardBudgetPersonDetail(awardBudgetPerson);
					}
				}
				/*
				 * awardBudgetDetail.setBudgetDetailCalcAmounts(
				 * awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(awardBudgetDetail.
				 * getBudgetDetailId())); awardBudgetDao.deleteAllCalcAmount(awardBudgetDetail.
				 * getBudgetDetailCalcAmounts());
				 * awardBudgetDetail.getBudgetDetailCalcAmounts().clear();
				 */
				awardBudgetDao.deleteBudgetDetail(awardBudgetDetail);
				awardBudgetDetail.getBudgetDetailCalcAmounts().clear();
				updatedAwardBudgetDetails.remove(awardBudgetDetail);
			}
		});
		awardBudgetDetails.stream().forEach(awardBudgetDetail -> {
			if (isSystemGeneratedCostEnabled) {
				if (updatedAwardBudgetDetails.size() <= sysGeneratedCESize) {
					awardBudgetDao.deleteBudgetDetail(awardBudgetDetail);
					// awardBudgetDetail.getBudgetDetailCalcAmounts().clear();
					updatedAwardBudgetDetails.remove(awardBudgetDetail);
				}
			}
		});
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

	protected void updateBudgetPeriods(List<AwardBudgetPeriod> budgetPeriods, int checkPeriod, boolean deletePeriod) {
		for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
			Integer budPeriod = budgetPeriod.getBudgetPeriod();
			if (budPeriod >= checkPeriod) {
				int newPeriod = 0;
				if (deletePeriod) {
					newPeriod = budPeriod - 1;
				} else {
					newPeriod = budPeriod + 1;
				}
				budgetPeriod.setBudgetPeriod(newPeriod);
				List<AwardBudgetDetail> budgetLineItems = budgetPeriod.getBudgetDetails();
				for (AwardBudgetDetail periodLineItem : budgetLineItems) {
					periodLineItem.setBudgetPeriod(newPeriod);
				}
			}
		}
	}

	@Override
	public String fetchBudgetSummaryTable(Integer awardBudgetId) {
		Set<String> budgetCategoryCodes = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		List<AwardBudgetSummaryVO> awardBudgetSummaryVOs = new ArrayList<>();
		AwardBudgetSummary budgetSummary = new AwardBudgetSummary();
		List<AwardBudgetPeriodSummary> budgetPeriodSummaries = new ArrayList<>();
		AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(awardBudgetId);
		budgetHeader.setEnableAwardBudgetVirementCalculation(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AWARD_BUDGET_VIREMENT_CALCULATION));
		setBudgetDetails(budgetHeader);
		fetchAwardBudgetPeriods(budgetHeader);
		List<AwardBudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
		List<Integer> budgetPeriodIds = new ArrayList<>();
		for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
			budgetPeriodIds.add(budgetPeriod.getBudgetPeriodId());
			budgetDetails.addAll(budgetPeriod.getBudgetDetails());
			AwardBudgetSummaryVO budgetSummaryVO = new AwardBudgetSummaryVO();
			budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
			budgetSummaryVO.setLineItemCost(budgetPeriod.getTotalCost());
			awardBudgetSummaryVOs.add(budgetSummaryVO);
		}
		budgetSummary.setBudgetSummaryVOs(awardBudgetSummaryVOs);
		for (AwardBudgetDetail budgetDetail : budgetDetails) {
			if (budgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
				costElements.add(budgetDetail.getCostElementCode());
			} else {
				budgetCategoryCodes.add(budgetDetail.getBudgetCategoryCode());
			}
		}
		BigDecimal periodTotalSum = BigDecimal.ZERO;
		for (String budgetCategoryCode : budgetCategoryCodes) {
			AwardBudgetPeriodSummary budgetPeriodSummary = new AwardBudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			List<AwardBudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			BudgetCategory budgetCategory = proposalBudgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
			budgetPeriodSummary.setBudgetCategory(budgetCategory.getDescription());
			budgetPeriodSummary.setSortOrder(budgetCategory.getSortOrder());
			for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				for (AwardBudgetDetail budgetDetail : budgetDetails) {
					if (budgetCategoryCode.equals(budgetDetail.getBudgetCategoryCode())
							&& (budgetPeriod.getBudgetPeriodId().equals(budgetDetail.getBudgetPeriodId()))
							&& !budgetCategoryCode.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
						totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				AwardBudgetSummaryVO budgetSummaryVO = new AwardBudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				totalLineItemCosts.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			budgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setBudgetCategoryCode(budgetCategory.getCode());
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		for (String costElement : costElements) {
			AwardBudgetPeriodSummary budgetPeriodSummary = new AwardBudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			List<AwardBudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			budgetPeriodSummary.setBudgetCategory(proposalBudgetDao.fetchCostElementName(costElement));
			for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				for (AwardBudgetDetail budgetDetail : budgetDetails) {
					if (costElement.equals(budgetDetail.getCostElementCode())
							&& (budgetPeriod.getBudgetPeriodId().equals(budgetDetail.getBudgetPeriodId()))
							&& budgetDetail.getBudgetCategoryCode()
									.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
						totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				AwardBudgetSummaryVO budgetSummaryVO = new AwardBudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				totalLineItemCosts.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			budgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalLineItemCostSum);
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		budgetSummary.setBudgetPeriodSummaries(budgetPeriodSummaries);
		budgetSummary.setPeriodTotalSum(periodTotalSum);
		budgetSummary.setBudgetHeader(budgetHeader);
		budgetSummary.setPeriodTotalFundRequestedSum(periodTotalSum);
		budgetSummary.setIsBudgetVersionEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS));
		budgetSummary.setIsBudgetSummaryEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_SUMMARY_ENABLED));
		budgetSummary.setIsShowBudgetOHRatePercentage(commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
		budgetSummary.setEnableCostShareStatus(commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS));
		budgetSummary.setShowAwardBudgetFieldForSap(commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_FIELDS_FOR_SAP));
		budgetSummary.setEnabledCampusFlagAward(commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_AWARD));
		return commonDao.convertObjectToJSON(budgetSummary);
	}

	@Override
	public String getSyncAwardBudgetRates(AwardBudgetVO vo) {
		try {
			AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getAwardBudgetId());
			budgetHeader.setInitialAvailableFund(calculateAvailableFund(budgetHeader.getAwardId(), budgetHeader.getAwardNumber(), true, budgetHeader.getAvailableFundType()));
			budgetHeader.setAvailableFund(budgetHeader.getInitialAvailableFund().subtract(budgetHeader.getTotalCost()));
			List<AwardRates> rates = getAwardRatesByBudgetId(vo.getAwardBudgetId());
			if (!rates.isEmpty()) {
				deleteAwardBudgetRate(rates);
			}
			awardService.updateAwardDocumentUpdateUserAndTimestamp(budgetHeader.getAwardId(), vo.getUserName());
			vo = setAwardRates(budgetHeader);
		} catch (Exception e) {
			logger.info("error in getSyncAwardBudgetRates", e);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private AwardBudgetVO setAwardRates(AwardBudgetHeader budgetHeader) {
		AwardBudgetVO vo = new AwardBudgetVO();
		fetchAwardBudgetPeriods(budgetHeader);
		budgetHeader.getAwardRates().clear();
		Award award = awardDao.fetchAwardByAwardId(budgetHeader.getAwardId().toString());
		Set<String> rateClassTypes = new HashSet<>();
		List<AwardRates> awardBudgetRates = fetchFilteredAwardRates(budgetHeader, award.getActivityTypeCode(),
				rateClassTypes);
		saveOrUpdateAwardBudgetRates(awardBudgetRates);
		budgetHeader.getAwardRates().addAll(awardBudgetRates);
		if (budgetHeader.getIsAutoCalc()) {
			calculate(budgetHeader, null, award.getActivityTypeCode());
		}
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(budgetHeader.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
		}
		vo.setAwardBudgetList(budgetHeaderDetails);
		vo.setAward(award);
		vo.setIsCreated(true);
		vo.setAwardBudgetHeader(budgetHeader);
		vo.setAwardRates(awardBudgetRates);
		setRateClassType(awardBudgetRates, vo);
		return vo;
	}
	
	private void setRateClassType(List<AwardRates> awardRates, AwardBudgetVO vo) {
		Set<String> rateClassTypes = new HashSet<>();
		if (awardRates != null && !awardRates.isEmpty()) {
			for (AwardRates awardRate : awardRates) {
				rateClassTypes.add(awardRate.getRateClass().getDescription());
			}
		}
		vo.setRateClassTypes(rateClassTypes);
	}
	
	@Override
	public List<AwardRates> saveOrUpdateAwardBudgetRates(List<AwardRates> awardRates) {
		List<AwardRates> newAwardRates = new ArrayList<>();
		for (AwardRates awardRate : awardRates) {
			awardBudgetDao.saveOrUpdateAwardBudgetRate(awardRate);
			newAwardRates.add(awardRate);
		}
		return newAwardRates;
	}

	@Override
	public List<AwardRates> getAwardRatesByBudgetId(Integer budgetId) {
		return awardBudgetDao.fetchAwardRatesByBudgetId(budgetId);
	}

	@Override
	public void deleteAwardBudgetRate(List<AwardRates> awardRates) {
		awardBudgetDao.deleteAwardBudgetRate(awardRates);
	}

	@Override
	public List<AwardRates> fetchFilteredAwardRates(AwardBudgetHeader budget, String activityTypeCode,
			Set<String> rateClassTypes) {
		List<AwardRates> awardRates = new ArrayList<>();
		String awardLeadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(budget.getAwardId());
		InstituteRate rateMTDC = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(budget.getStartDate(), activityTypeCode,
				"1", "1", awardLeadUnitNumber);
		if (rateMTDC != null) {
			logger.info("rateMTDC : {}", rateMTDC.getInstituteRate());
			awardRates.add(prepareAwardRate(rateMTDC, budget, rateClassTypes));
		}
		fetchEmployeeBenifitsRates(awardRates, budget.getStartDate(), activityTypeCode, budget, rateClassTypes);
		InstituteRate rateInflation = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(budget.getStartDate(),
				activityTypeCode, "7", "1", awardLeadUnitNumber);
		if (rateInflation != null) {
			logger.info("rateInflation : {}", rateInflation.getInstituteRate());
			awardRates.add(prepareAwardRate(rateInflation, budget, rateClassTypes));
		}
		List<InstituteRate> instituteRates = proposalBudgetDao.filterInstituteRateByDateRange(budget.getStartDate(),
				budget.getEndDate(), activityTypeCode, budget.getOnOffCampusFlag());
		if (instituteRates != null && !instituteRates.isEmpty()) {
			for (InstituteRate instituteRate : instituteRates) {
				awardRates.add(prepareAwardRate(instituteRate, budget, rateClassTypes));
			}
		}
		if (rateClassTypes != null && !rateClassTypes.isEmpty()) {
			List<String> rateClassCodes = proposalBudgetDao.fetchRateClassCodesNotInParams(rateClassTypes);
			if (rateClassCodes != null && !rateClassCodes.isEmpty()) {
				for (String rateClassCode : rateClassCodes) {
					List<String> rateTypeCodes = proposalBudgetDao.fetchRateTypeCodesByRateClassCode(rateClassCode);
					if (rateTypeCodes != null && !rateTypeCodes.isEmpty()) {
						for (String rateTypeCode : rateTypeCodes) {
							InstituteRate otherRate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(
									budget.getStartDate(), activityTypeCode, rateClassCode, rateTypeCode, awardLeadUnitNumber);
							if (otherRate != null) {
								logger.info("otherRate : {}", otherRate.getInstituteRate());
								awardRates.add(prepareAwardRate(otherRate, budget, rateClassTypes));
							}
						}
					}
				}
			}
		}
		return awardRates;
	}

	public AwardRates prepareAwardRate(InstituteRate instituteRate, AwardBudgetHeader budget,
			Set<String> rateClassTypes) {
		AwardRates awardRate = new AwardRates();
		awardRate.setApplicableRate(instituteRate.getInstituteRate());
		awardRate.setFiscalYear(instituteRate.getFiscalYear());
		awardRate.setInstituteRate(instituteRate.getInstituteRate());
		String flag = instituteRate.getOnOffCampusFlag();
		if (flag.equalsIgnoreCase("N")) {
			awardRate.setOnOffCampusFlag(true);
		} else {
			awardRate.setOnOffCampusFlag(false);
		}
		awardRate.setBudgetHeaderId(budget.getBudgetId());
		awardRate.setRateClassCode(instituteRate.getRateClassCode());
		awardRate.setRateTypeCode(instituteRate.getRateTypeCode());
		awardRate.setStartDate(instituteRate.getStartDate());
		awardRate.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		awardRate.setUpdateUser(budget.getUpdateUser());
		awardRate.setActivityTypeCode(instituteRate.getActivityTypeCode());
		awardRate.setRateClass(instituteRate.getRateClass());
		awardRate.setRateType(instituteRate.getRateType());
		awardRate.setActivityType(instituteRate.getActivityType());
		rateClassTypes.add(instituteRate.getRateClass().getDescription());
		return awardRate;
	}

	@Override
	public String resetAwardRates(Integer budgetId, String userName) {
		AwardBudgetVO vo = new AwardBudgetVO();
		try {
			AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
			budgetHeader.setInitialAvailableFund(calculateAvailableFund(budgetHeader.getAwardId(), budgetHeader.getAwardNumber(), true, budgetHeader.getAvailableFundType()));
			budgetHeader.setAvailableFund(budgetHeader.getInitialAvailableFund().subtract(budgetHeader.getTotalCost()));
			resetAwardRates(budgetHeader, vo);
			awardService.updateAwardDocumentUpdateUserAndTimestamp(budgetHeader.getAwardId(), userName);
		} catch (Exception e) {
			logger.info("error in reset Award Rates", e);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private AwardBudgetVO resetAwardRates(AwardBudgetHeader budgetHeader, AwardBudgetVO vo) {
		fetchAwardBudgetPeriods(budgetHeader);
		Award award = awardDao.fetchAwardByAwardId(budgetHeader.getAwardId().toString());
		List<AwardRates> awardBudgetRates = getAwardRatesByBudgetId(budgetHeader.getBudgetId());
		List<AwardRates> awardRates = new ArrayList<>();
		for (AwardRates rate : awardBudgetRates) {
			rate.setApplicableRate(rate.getInstituteRate());
			awardRates.add(rate);
		}
		saveOrUpdateAwardBudgetRates(awardRates);
		if (budgetHeader.getIsAutoCalc()) {
			calculate(budgetHeader, null, award.getActivityTypeCode());
		}
		List<AwardBudgetHeader> budgetHeaders = awardBudgetDao.fetchAwardBudgetHeaderByAwardId(budgetHeader.getAwardId());
		List<AwardBudgetHeaderDetail> budgetHeaderDetails = new ArrayList<>();
		for (AwardBudgetHeader budget : budgetHeaders) {
			budgetHeaderDetails.add(prepareAwardBudgetHeaderDetail(budget));
		}
		vo.setAwardBudgetList(budgetHeaderDetails);
		vo.setAward(award);
		vo.setIsCreated(true);
		budgetHeader.getAwardRates().addAll(awardRates);
		vo.setAwardBudgetHeader(budgetHeader);
		vo.setAwardRates(awardRates);
		setRateClassType(awardRates, vo);
		return vo;
	}

	@Override
	public String applayAwardRates(Integer budgetId, List<AwardRates> awardRates, String userName) {
		AwardBudgetVO vo = new AwardBudgetVO();
		try {
			AwardBudgetHeader budgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
			Award award = awardDao.getAwardDetailsById(budgetHeader.getAwardId());
			applayAwardBudgetRates(budgetHeader, vo, awardRates);
			fetchAwardBudgetPeriods(budgetHeader);
			calculate(budgetHeader, null, award.getActivityTypeCode());
			updateBudgetHeader(budgetHeader);
			vo.setAwardBudgetHeader(budgetHeader);
			awardService.updateAwardDocumentUpdateUserAndTimestamp(budgetHeader.getAwardId(), userName);
		} catch (Exception e) {
			logger.info("error in apply Award Rates", e);
		}
		return commonDao.convertObjectToJSON(vo);
	}
	
	private void applayAwardBudgetRates(AwardBudgetHeader budgetHeader, AwardBudgetVO vo, List<AwardRates> awardRates) {
		List<AwardRates> appliedAwardRates = new ArrayList<>();
		appliedAwardRates = saveOrUpdateAwardBudgetRates(awardRates);
		budgetHeader.getAwardRates().addAll(appliedAwardRates);
		setRateClassType(appliedAwardRates, vo);
		vo.setIsAutoCalculationEnabled(budgetHeader.getIsAutoCalc());
		vo.setAwardRates(appliedAwardRates);
	}
	
	private void calculateAwardPeriod(AwardBudgetHeader budgetHeader, AwardBudgetPeriod budgetPeriod,
			String activityTypeCode) {
		BigDecimal totalFringeCost = BigDecimal.ZERO;
		BigDecimal totalFandACost = BigDecimal.ZERO;
		BigDecimal totalLineItemCost = BigDecimal.ZERO;
		List<AwardBudgetDetail> budgetDetailsList = budgetPeriod.getBudgetDetails();
		if (budgetDetailsList != null && !budgetDetailsList.isEmpty()) {
			for (AwardBudgetDetail budgetItemDetail : budgetDetailsList) {
				if (!budgetItemDetail.getIsSystemGeneratedCostElement()) {
					List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmount = awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(budgetItemDetail.getBudgetDetailId());
					List<AwardBudgetDetailCalcAmount> updatedAwardBudgetCalcAmounts = new ArrayList<>(awardBudgetDetailCalcAmount);
					Collections.copy(updatedAwardBudgetCalcAmounts, awardBudgetDetailCalcAmount);
					for (AwardBudgetDetailCalcAmount amount : awardBudgetDetailCalcAmount) {
						if (!amount.getRateClassCode().equals("7")) {
							updatedAwardBudgetCalcAmounts.remove(amount);
						}
					}
					budgetItemDetail.getBudgetDetailCalcAmounts().clear();
					budgetItemDetail.getBudgetDetailCalcAmounts().addAll(updatedAwardBudgetCalcAmounts);
					BigDecimal fringeCostForCE;
					BigDecimal fandACostForCE;
					BigDecimal lineItemCost = budgetItemDetail.getLineItemCost();
					totalLineItemCost = totalLineItemCost.add(lineItemCost);
					deleteAwardBudgetCalcAmountByAwdBudgetDetailId(budgetItemDetail.getBudgetDetailId());
					fringeCostForCE = calculateFringeCostForCE(budgetHeader, budgetPeriod, budgetItemDetail,
							lineItemCost, activityTypeCode);
					fandACostForCE = calculateFandACostForCE(budgetHeader, budgetPeriod, budgetItemDetail,
							baseCostForCalculatingFandA(budgetItemDetail, fringeCostForCE), activityTypeCode);
					totalFringeCost = totalFringeCost.add(fringeCostForCE);
					totalFandACost = totalFandACost.add(fandACostForCE);
				}
			}
			for (AwardBudgetDetail budgetItemDetail : budgetDetailsList) {
				if (budgetItemDetail.getIsSystemGeneratedCostElement()) {
					List<AwardBudgetDetailCalcAmount> newAwardBudgetCalcAmounts = budgetItemDetail.getBudgetDetailCalcAmounts();
					List<AwardBudgetDetailCalcAmount> updatedAwardBudgetDetailCalcAmount = new ArrayList<>(newAwardBudgetCalcAmounts);
					Collections.copy(updatedAwardBudgetDetailCalcAmount, newAwardBudgetCalcAmounts);
					for (AwardBudgetDetailCalcAmount amount : newAwardBudgetCalcAmounts) {
						if (!amount.getRateClassCode().equals("7")) {
							updatedAwardBudgetDetailCalcAmount.remove(amount);
						}
					}
					budgetItemDetail.getBudgetDetailCalcAmounts().clear();
					budgetItemDetail.getBudgetDetailCalcAmounts().addAll(updatedAwardBudgetDetailCalcAmount);

					if (Constants.BUDGET_FRINGE_ON.equals(budgetItemDetail.getSystemGeneratedCEType())
							|| Constants.BUDGET_FRINGE_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						budgetItemDetail.setLineItemCost(totalFringeCost.setScale(2, RoundingMode.HALF_UP));
					}
					if (Constants.BUDGET_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType())
							|| Constants.BUDGET_OH_OFF.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						budgetItemDetail.setLineItemCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
					}
					if (Constants.BUDGET_RESEARCH_OH_ON.equals(budgetItemDetail.getSystemGeneratedCEType())) {
						budgetItemDetail.setLineItemCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
					}
				}
			}
			budgetPeriod.setTotalDirectCost(totalLineItemCost.add(totalFringeCost).setScale(2, RoundingMode.HALF_UP));
			budgetPeriod.setTotalIndirectCost(totalFandACost.setScale(2, RoundingMode.HALF_UP));
			budgetPeriod.setTotalCost(
					totalLineItemCost.add(totalFringeCost).add(totalFandACost).setScale(2, RoundingMode.HALF_UP));
		}

	}

	private void calculate(AwardBudgetHeader budgetHeader, Integer period, String activityTypeCode) {
		List<AwardBudgetPeriod> budgetPeriodsList = budgetHeader.getBudgetPeriods();
		if (period != null) {
			for (AwardBudgetPeriod budgetPeriod : budgetPeriodsList) {
				if (period.equals(budgetPeriod.getBudgetPeriod())) {
					calculateAwardPeriod(budgetHeader, budgetPeriod, activityTypeCode);
				}
			}
		} else {
			for (AwardBudgetPeriod budgetPeriod : budgetPeriodsList) {
				calculateAwardPeriod(budgetHeader, budgetPeriod, activityTypeCode);
			}
		}
	}

	private BigDecimal calculateFandACostForCE(AwardBudgetHeader budgetHeader, AwardBudgetPeriod budgetPeriod,
			AwardBudgetDetail budgetDetail, BigDecimal fringeWithLineItemCost, String activityTypeCode) {
		BigDecimal fandACost = BigDecimal.ZERO;
		Integer budgetId = budgetHeader.getBudgetId();
		CostElement costElement = budgetDetail.getCostElement();
		costElement = awardBudgetDao.fetchCostElementsById(costElement.getCostElement());
		AwardBudgetDetailCalcAmount budgetCalculatedAmount = null;
		String ohRateClassTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_OH_RATE_CLASS_TYPE_CODE);
		String rateTypeCode = commonDao.getParameterValueAsString(Constants.DEFAULT_RATE_TYPE_CODE);
		List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
		if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
			for (ValidCeRateType ceRateType : ceRateTypes) {
				if (ceRateType.getRateClassCode().equals(budgetHeader.getRateClassCode())
						&& ceRateType.getRateTypeCode().equalsIgnoreCase(budgetHeader.getRateTypeCode())) {
					AwardRates applicableRate = awardBudgetDao.fetchApplicableAwardRate(budgetId,
							budgetPeriod.getStartDate(), ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(),
							activityTypeCode);
					if (applicableRate != null
							&& applicableRate.getRateClass().getRateClassTypeCode().equals(ohRateClassTypeCode)
							&& applicableRate.getRateTypeCode().equals(rateTypeCode)) {
						BigDecimal validRate = BigDecimal.ZERO;
						validRate = validRate.add(applicableRate.getApplicableRate());
						if (validRate.compareTo(BigDecimal.ZERO) > 0) {
							BigDecimal hundred = new BigDecimal(100);
							BigDecimal percentageFactor = validRate.divide(hundred);
							BigDecimal calculatedCost = (fringeWithLineItemCost.multiply(percentageFactor));
							fandACost = fandACost.add(calculatedCost);
							budgetCalculatedAmount = getNewAwardBudgetCalculatedAmount(budgetPeriod, budgetDetail,
									applicableRate);
							budgetCalculatedAmount
									.setCalculatedCost(calculatedCost.setScale(2, RoundingMode.HALF_UP).setScale(2));
							budgetDetail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
						}
					}
				}

			}
		}
		return fandACost;
	}

	public AwardBudgetHeader calculateAwardBudgetDetails(AwardBudgetHeader budgetHeader, String activityTypeCode) {
		calculate(budgetHeader, null, activityTypeCode);
		return budgetHeader;
	}

	private BigDecimal baseCostForCalculatingFandA(AwardBudgetDetail budgetItemDetail, BigDecimal fringeCostForCE) {
		BigDecimal calculationBaseAmount;
		BigDecimal lineItemCost = budgetItemDetail.getLineItemCost();
		// If personal lineItem
		if ("P".equals(budgetItemDetail.getBudgetCategory().getBudgetCategoryTypeCode())) {
			// if person,
			// base amount = (line item cost + fringe benefit) * percentage charged
			// Non issue : Below logic just a workaround
			// actual need to calculate Fringe for each personal line, here fringe is of
			// total person
			// As workaround, just taking average of % charged for multiple person
			BigDecimal hundred = new BigDecimal(100);
			calculationBaseAmount = fringeCostForCE.add(lineItemCost);
			BigDecimal percentageCharged = getPersonalPercentageCharged(budgetItemDetail);
			percentageCharged = percentageCharged.divide(hundred, 2, RoundingMode.HALF_UP);
			calculationBaseAmount = (calculationBaseAmount.multiply(percentageCharged));
		} else {
			calculationBaseAmount = lineItemCost;
		}
		return calculationBaseAmount;
	}

	private BigDecimal getPersonalPercentageCharged(AwardBudgetDetail budgetItemDetail) {
		BigDecimal totalPercentageCharged = BigDecimal.ZERO;
		BigDecimal numberOfPersonWithEffort = BigDecimal.ZERO;
		if (budgetItemDetail.getPersonsDetails() != null && !budgetItemDetail.getPersonsDetails().isEmpty()) {
			List<AwardBudgetPersonalDetail> budgetPersonalDetails = budgetItemDetail.getPersonsDetails();
			for (AwardBudgetPersonalDetail budgetPersonalDetail : budgetPersonalDetails) {
				if (budgetPersonalDetail.getPercentageCharged() != null) {
					totalPercentageCharged = totalPercentageCharged.add(budgetPersonalDetail.getPercentageCharged());
					numberOfPersonWithEffort = numberOfPersonWithEffort.add(BigDecimal.ONE);
				}
				if (totalPercentageCharged != BigDecimal.ZERO) {
					totalPercentageCharged = totalPercentageCharged.divide(numberOfPersonWithEffort, 2,
							RoundingMode.HALF_UP);
				}
			}
		}
		if (totalPercentageCharged == BigDecimal.ZERO) {
			totalPercentageCharged = new BigDecimal(100);
		}
		return totalPercentageCharged;
	}

	private BigDecimal calculateFringeCostForCE(AwardBudgetHeader budgetHeader, AwardBudgetPeriod budgetPeriod,
			AwardBudgetDetail budgetDetail, BigDecimal lineItemCost, String activityTypeCode) {
		BigDecimal fringeCost = BigDecimal.ZERO;
		Integer budgetId = budgetHeader.getBudgetId();
		Date budgetPeriodStartDate = new Date(budgetPeriod.getStartDate().getTime());
		Date budgetPeriodEndDate = new Date(budgetPeriod.getEndDate().getTime());
		CostElement costElement = budgetDetail.getCostElement();
		costElement = awardBudgetDao.fetchCostElementsById(costElement.getCostElement());
		BigDecimal perDayCost = lineItemCost.divide(
				new BigDecimal(((budgetPeriodEndDate.getTime() - budgetPeriodStartDate.getTime()) / 86400000 + 1)), 4,
				RoundingMode.HALF_UP);
		AwardBudgetDetailCalcAmount budgetCalculatedAmount = null;
		List<ValidCeRateType> ceRateTypes = costElement.getValidCeRateTypes();
		if (ceRateTypes != null && !ceRateTypes.isEmpty()) {
			int numberOfDays = (int) ((budgetPeriodEndDate.getTime() - budgetPeriodStartDate.getTime()) / 86400000) + 1;
			if (numberOfDays == 0) {
				numberOfDays = 1;
			}
			for (ValidCeRateType ceRateType : ceRateTypes) {
				AwardRates applicableRate = awardBudgetDao.fetchApplicableAwardRate(budgetId, budgetPeriod.getStartDate(),
						ceRateType.getRateClassCode(), ceRateType.getRateTypeCode(), activityTypeCode);
				if (applicableRate != null && (applicableRate.getRateClass().getRateClassTypeCode().equals("E"))) {
					BigDecimal validRate = BigDecimal.ZERO;
					validRate = validRate.add(applicableRate.getApplicableRate());
					if (validRate.compareTo(BigDecimal.ZERO) > 0) {
						BigDecimal hundred = new BigDecimal(100);
						BigDecimal percentageFactor = validRate.divide(hundred, 2, RoundingMode.HALF_UP);
						BigDecimal calculatedCost = ((perDayCost.multiply(percentageFactor))
								.multiply(new BigDecimal(numberOfDays))).setScale(2, RoundingMode.HALF_UP).setScale(2);
						fringeCost = fringeCost.add(calculatedCost);
						budgetCalculatedAmount = getNewAwardBudgetCalculatedAmount(budgetPeriod, budgetDetail,
								applicableRate);
						budgetCalculatedAmount.setCalculatedCost(calculatedCost);
						budgetDetail.getBudgetDetailCalcAmounts().add(budgetCalculatedAmount);
					}
				}
			}
		}
		return fringeCost;
	}

	public void fetchEmployeeBenifitsRates(List<AwardRates> awardRates, Timestamp startDate,
			String activityTypeCode, AwardBudgetHeader budget, Set<String> rateClassTypes) {
		String awardLeadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(budget.getAwardId());
		InstituteRate fullTimeRate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, "5",
				"1", awardLeadUnitNumber);
		if (fullTimeRate != null) {
			logger.info("fullTimeRate : {}", fullTimeRate.getInstituteRate());
			awardRates.add(prepareAwardRate(fullTimeRate, budget, rateClassTypes));
		}
		InstituteRate partTimeRate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, "5",
				"2", awardLeadUnitNumber);
		if (partTimeRate != null) {
			logger.info("partTimeRate : {}", partTimeRate.getInstituteRate());
			awardRates.add(prepareAwardRate(partTimeRate, budget, rateClassTypes));
		}
		InstituteRate stipendsRate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, "5",
				"4", awardLeadUnitNumber);
		if (stipendsRate != null) {
			logger.info("stipendsRate : {}", stipendsRate.getInstituteRate());
			awardRates.add(prepareAwardRate(stipendsRate, budget, rateClassTypes));
		}
		InstituteRate jHURate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, "5", "5", awardLeadUnitNumber);
		if (jHURate != null) {
			logger.info("jHURate : {}", jHURate.getInstituteRate());
			awardRates.add(prepareAwardRate(jHURate, budget, rateClassTypes));
		}
		InstituteRate wagesRate = awardBudgetDao.fetchInstituteRateByUnitAndDateLessthanMax(startDate, activityTypeCode, "5", "6", awardLeadUnitNumber);
		if (wagesRate != null) {
			logger.info("wagesRate : {}", wagesRate.getInstituteRate());
			awardRates.add(prepareAwardRate(wagesRate, budget, rateClassTypes));
		}
	}

	@Override
	public String saveOrUpdateAwardBudgetCalcAmount(List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmounts) {
		List<AwardBudgetDetailCalcAmount> newAwardBudgetDetailCalcAmounts = new ArrayList<>();
		for (AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount : awardBudgetDetailCalcAmounts) {
			awardBudgetDao.saveOrUpdateAwardBudgetDetailCalcAmount(awardBudgetDetailCalcAmount);
			newAwardBudgetDetailCalcAmounts.add(awardBudgetDetailCalcAmount);
		}
		return commonDao.convertObjectToJSON(newAwardBudgetDetailCalcAmounts);
	}

	@Override
	public String getAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId) {
		return commonDao.convertObjectToJSON(awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(detailId));
	}

	@Override
	public void deleteAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId) {
		List<AwardBudgetDetailCalcAmount> budgetCalcAmounts = awardBudgetDao.getAwardBudgetCalcAmountByAwdBudgetDetailId(detailId);
		for (AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount : budgetCalcAmounts) {
			awardBudgetDao.deleteAwardBudgetCalcAmountByAwdBudgetDetailId(awardBudgetDetailCalcAmount);
		}
	}

	@Override
	public Boolean checkAwardBudgetPersonAddedInBudget(Integer budgetPersonId) {
		return awardBudgetDao.checkAwardBudgetPersonAddedInBudget(budgetPersonId);
	}

	@Override
	public BigDecimal calculateVirement(Integer budgetPeriodId) {
		HashMap<String, List<HashMap<String, Object>>> awardBudgetLineItemCategories = new HashMap<>();
		List<HashMap<String, Object>> awardLineItemDetails = new ArrayList<>();
		List<BigDecimal> categoryWisecost = new ArrayList<>();
		List<BigDecimal> singleCategoryCost = new ArrayList<>();
		BigDecimal lineItemCostChange = BigDecimal.ZERO;
		BigDecimal lineItemCostForSameCategory = BigDecimal.ZERO;
		BigDecimal lineItemCostForDiffCategory = BigDecimal.ZERO;
		List<AwardBudgetDetail> awardBudgetDetails = awardBudgetDao.fetchAwardBudgetDetailByPeriodId(budgetPeriodId);
		for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
			HashMap<String, Object> awardLineItemDetail = new HashMap<>();
//			if (awardBudgetDetail.getPrevLineItemCost() == null) {
//				commented by arjun
//				awardBudgetDetail.setPrevLineItemCost(BigDecimal.ZERO);
//				awardBudgetDetail.setPrevLineItemCost(null);
//			}
			if ((!awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST))) {
				lineItemCostChange = BigDecimal.ZERO;
				if (awardBudgetDetail.getPrevLineItemCost() == null) {
					lineItemCostChange = lineItemCostChange.add(awardBudgetDetail.getLineItemCost());
				} else {
					lineItemCostChange = lineItemCostChange.add(awardBudgetDetail.getLineItemCost().subtract(awardBudgetDetail.getPrevLineItemCost()));
				}
				awardLineItemDetail.put(LINE_ITEM_COST_CHANGE, lineItemCostChange);
				awardLineItemDetail.put("budgetDetailId", awardBudgetDetail.getBudgetDetailId());
				awardLineItemDetail.put(BUDGET_CATEGORY_CODE, awardBudgetDetail.getBudgetCategoryCode());
				awardLineItemDetails.add(awardLineItemDetail);
			}
		}
		if (awardLineItemDetails != null && !awardLineItemDetails.isEmpty()) {
		for (int i = 0; i < awardLineItemDetails.size(); i++) {
			List<HashMap<String, Object>> awardLineItems = new ArrayList<>();
			for (int j = 0; j < awardLineItemDetails.size(); j++) {
				if (awardLineItemDetails.get(i).get(BUDGET_CATEGORY_CODE).equals(awardLineItemDetails.get(j).get(BUDGET_CATEGORY_CODE))) {
					awardLineItems.add(awardLineItemDetails.get(j));
				}
			}
			awardBudgetLineItemCategories.put(awardLineItemDetails.get(i).get(BUDGET_CATEGORY_CODE).toString(), awardLineItems);
		}
		for (Map.Entry<String, List<HashMap<String, Object>>> awardBudgetLineItemCategory : awardBudgetLineItemCategories.entrySet()) {
			if (awardBudgetLineItemCategory.getValue().size() > 1) {
				for (int index = 0; index < awardBudgetLineItemCategory.getValue().size(); index++) {
					lineItemCostForSameCategory = lineItemCostForSameCategory.add((BigDecimal) awardBudgetLineItemCategory.getValue().get(index).get(LINE_ITEM_COST_CHANGE));
				}
				if(lineItemCostForSameCategory.doubleValue() < 0.0) {
					lineItemCostForSameCategory = BigDecimal.ZERO;
				} else {
					categoryWisecost.add(lineItemCostForSameCategory);	
					lineItemCostForSameCategory = BigDecimal.ZERO;
				}
			} else {
					lineItemCostForDiffCategory = (BigDecimal) awardBudgetLineItemCategory.getValue().get(0).get(LINE_ITEM_COST_CHANGE);
				if(lineItemCostForDiffCategory.doubleValue() < 0.0) {
					lineItemCostForDiffCategory =  BigDecimal.ZERO;
				} else {
				 singleCategoryCost.add(lineItemCostForDiffCategory);
				 lineItemCostForDiffCategory = BigDecimal.ZERO;
				}
			}
		}
		lineItemCostForSameCategory = categoryWisecost.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
		lineItemCostForDiffCategory = singleCategoryCost.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
		lineItemCostChange = lineItemCostForSameCategory.add(lineItemCostForDiffCategory);
		}
		return lineItemCostChange;
	}

	private BigDecimal calculateAwardBudgetVirment(Integer budgetId, BigDecimal lineItemChange) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
		BigDecimal virement = BigDecimal.ZERO;
		BigDecimal hundred = new BigDecimal(100);
		if (lineItemChange.compareTo(BigDecimal.ZERO) > 0
				&& awardBudgetHeader.getTotalDirectCost().compareTo(BigDecimal.ZERO) > 0) {
			virement = lineItemChange.multiply(hundred);
			virement = virement.divide(awardBudgetHeader.getTotalDirectCost(), 2, RoundingMode.HALF_UP);
		}
		return virement;
	}

	private BigDecimal calculateCumilativeVirement(BigDecimal virement) {
		BigDecimal cumilativeVirement = BigDecimal.ZERO;
		cumilativeVirement = cumilativeVirement.add(virement);
		return cumilativeVirement;
	}

	@Override
	public BigDecimal calculateAvailableFund(Integer awardId, String awardNumber, boolean canIncludeInBudget, String availableFundType) {
		BigDecimal costShareAmountSum = BigDecimal.ZERO;
		BigDecimal availableFunds;
		BigDecimal referencedAmount= BigDecimal.ZERO;
		AwardBudgetFundType fundType = awardBudgetDao.getBudgetFundTypeByCode(availableFundType);
		if (fundType.getIsCostShareEnable().equals(Boolean.TRUE)) {
			List<AwardCostShare> awardCostShareTypes = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
			List<CostShareType> costShareTypes = datesAndAmountDao.getBudgetIncludedCostshareType(canIncludeInBudget);
			for (AwardCostShare awardCostShare : awardCostShareTypes) {
				for (CostShareType costShareType : costShareTypes) {
					if (costShareType.getCostShareTypeCode().equals(awardCostShare.getCostShareTypeCode())
							&& (awardCostShare.getCommitmentAmount() != null)) {
						costShareAmountSum = costShareAmountSum.add(awardCostShare.getCommitmentAmount());
					}
				}
			}
		}
		logger.info("Award awardNumber : {}", awardNumber);
		logger.info("Award Id: {}", awardId);
		AwardAmountInfo awardAmountInfo = fetchLatestAwardAmountInfo(awardId, awardNumber);

		if (awardAmountInfo != null) {
			referencedAmount = (BigDecimal) commonService.getValueBasedOnColumnName(awardAmountInfo, fundType.getReferanceColumn());
		}
		availableFunds = costShareAmountSum.add(referencedAmount).setScale(2, RoundingMode.HALF_UP).setScale(2);
		return availableFunds;
	}
	
	@Override
	public BigDecimal calculateObligatedTotal(Integer awardId, String awardNumber) {
		boolean canIncludeInBudget = true;
		BigDecimal costShareAmountSum = BigDecimal.ZERO;
		BigDecimal obligatedTotal = BigDecimal.ZERO;
		List<AwardCostShare> awardCostShareTypes = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		List<CostShareType> costShareTypes = datesAndAmountDao.getBudgetIncludedCostshareType(canIncludeInBudget);
		for (AwardCostShare awardCostShare : awardCostShareTypes) {
			for (CostShareType costShareType : costShareTypes) {
				if (costShareType.getCostShareTypeCode().equals(awardCostShare.getCostShareTypeCode())
						&& (awardCostShare.getCommitmentAmount() != null)) {
					costShareAmountSum = costShareAmountSum.add(awardCostShare.getCommitmentAmount());
				}
			}
		}
		AwardAmountInfo awardAmountInfo = fetchLatestAwardAmountInfo(awardId, awardNumber);
		if (awardAmountInfo != null) {
			obligatedTotal = obligatedTotal.add(awardAmountInfo.getObliDistributableAmount()); 
		}
		return obligatedTotal.setScale(2, RoundingMode.HALF_UP).setScale(2);
	}
	
	@Override
	public BigDecimal calculateAnticipatedAmount(Integer awardId, String awardNumber) {
		boolean canIncludeInBudget = true;
		BigDecimal costShareAmountSum = BigDecimal.ZERO;
		BigDecimal totalAnticipatedAmount = BigDecimal.ZERO;
		BigDecimal availableFunds = BigDecimal.ZERO;
		List<AwardCostShare> awardCostShareTypes = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		List<CostShareType> costShareTypes = datesAndAmountDao.getBudgetIncludedCostshareType(canIncludeInBudget);
		for (AwardCostShare awardCostShare : awardCostShareTypes) {
			for (CostShareType costShareType : costShareTypes) {
				if (costShareType.getCostShareTypeCode().equals(awardCostShare.getCostShareTypeCode())
						&& (awardCostShare.getCommitmentAmount() != null)) {
					costShareAmountSum = costShareAmountSum.add(awardCostShare.getCommitmentAmount());
				}
			}
		}
		AwardAmountInfo awardAmountInfo = fetchLatestAwardAmountInfo(awardId, awardNumber);
		if (awardAmountInfo != null) {
			totalAnticipatedAmount = totalAnticipatedAmount.add(awardAmountInfo.getAntDistributableAmount());
		}
		availableFunds = costShareAmountSum.add(totalAnticipatedAmount).setScale(2, RoundingMode.HALF_UP)
					.setScale(2);
		return availableFunds;
	}

	@Override
	public BigDecimal calculateCostShareIncludedInBudget(Integer awardId) {
		BigDecimal costShareAmountSum = BigDecimal.ZERO;
		boolean canIncludeInBudget = true;
		List<AwardCostShare> awardCostShareTypes = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		List<CostShareType> costShareTypes = datesAndAmountDao.getBudgetIncludedCostshareType(canIncludeInBudget);
		for (AwardCostShare awardCostShare : awardCostShareTypes) {
			for (CostShareType costShareType : costShareTypes) {
				if (costShareType.getCostShareTypeCode().equals(awardCostShare.getCostShareTypeCode())
						&& (awardCostShare.getCommitmentAmount() != null)) {
					costShareAmountSum = costShareAmountSum.add(awardCostShare.getCommitmentAmount());
				}
			}
		}
		return costShareAmountSum;
	}

	@Override
	public List<AwardBudgetPeriod> setPeriodData(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> awardBudgetPeriod = budgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId());
		for (AwardBudgetPeriod period : awardBudgetPeriod) {
			List<AwardBudgetDetail> awardBudgetDetails = budgetDao.fetchAwardBudgetDetailByPeriodId(period.getBudgetPeriodId());
			if (awardBudgetDetails != null && !awardBudgetDetails.isEmpty()) {
				Collections.sort(awardBudgetDetails, new AwardBudgetDetailComparatorBySortOrder());
				Collections.sort(awardBudgetDetails, new AwardBudgetDetailComparatorBySystemGenerated());
				period.setBudgetDetails(awardBudgetDetails);
			}
			awardBudgetHeader.getBudgetPeriods().add(period);
		}
		return awardBudgetHeader.getBudgetPeriods();
	}

	@Override
	public String getBudgetDetailsByAwardId(Integer awardId) {
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardId);
		if (awardBudgetHeader != null) {
			return fetchBudgetSummaryTable(awardBudgetHeader.getBudgetId());
		}
		return commonDao.convertObjectToJSON(new AwardBudgetSummary());
	}

	private void setBudgetDetails(AwardBudgetHeader awardBudgetHeader) {
		Award award = awardDao.getAwardDetailsById(awardBudgetHeader.getAwardId());
		awardBudgetHeader.setFundCode(award.getAccountNumber());
		awardBudgetHeader.setFundCenter(award.getFundCenter());
		awardBudgetHeader.setInitialAvailableFund(calculateAvailableFund(awardBudgetHeader.getAwardId(), awardBudgetHeader.getAwardNumber(), true, awardBudgetHeader.getAvailableFundType()));
		awardBudgetHeader.setAvailableFund(awardBudgetHeader.getInitialAvailableFund().subtract(awardBudgetHeader.getTotalCost()));
		boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		boolean isBudgetAssociatedWithManpower = commonDao
				.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		awardBudgetHeader.setManpowerEnabled(manpowerEnabled);
		awardBudgetHeader.setBudgetAssociatedWithManpower(isBudgetAssociatedWithManpower);
	}

	@Override
	public String deleteNonPersonnelLine(AwardBudgetVO awardVO) {
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_AB_PERSON_APPL_SAL_CALC)) {
			updateNonPersonalLineItemCost(awardVO);
		}
		awardBudgetDao.deleteBudgetNonPersonDetail(awardVO.getBudgetNonPersonDtlId());
		awardVO.setStatus(true);
		awardVO.setMessage("Budget non person deleted successfully");
		return commonDao.convertObjectToJSON(calculateAwardBudget(awardVO.getBudgetHeaderId()));
	}

	private void updateNonPersonalLineItemCost(AwardBudgetVO awardVO) {
		BigDecimal deletedNonPersonalLineItemCost = BigDecimal.ZERO;
		BigDecimal lineItemCostAfterDelete = BigDecimal.ZERO;
		AwardBudgetNonPersonDetail awardBudgetNonPersonDetail = awardBudgetDao.getAwardBudgetNonPersonalDetailsByPersonDetailId(awardVO.getBudgetNonPersonDtlId());
		if (awardBudgetNonPersonDetail != null && awardBudgetNonPersonDetail.getLineItemCost() != null) {
			deletedNonPersonalLineItemCost = awardBudgetNonPersonDetail.getLineItemCost();
			AwardBudgetDetail awardBudgetNonPersonalDetail = awardBudgetDao.getAwardBudgetDetailsByDetailId(awardBudgetNonPersonDetail.getAwardnonPersonBudgetDetail().getBudgetDetailId());
			if (awardBudgetNonPersonalDetail != null) {
				lineItemCostAfterDelete = (awardBudgetNonPersonalDetail.getLineItemCost().subtract(deletedNonPersonalLineItemCost));
				awardBudgetNonPersonalDetail.setLineItemCost(lineItemCostAfterDelete);
				awardBudgetDao.saveOrUpdateAwardBudgetLineItem(awardBudgetNonPersonalDetail);
			}
		}
	}

	@Override
	public String fetchAllBudgetFundType() {
		return commonDao.convertObjectToJSON(awardBudgetDao.fetchAllBudgetFundType());
	}

	@Override
	public AwardAmountInfo fetchLatestAwardAmountInfo(Integer awardId, String awardNumber) {
		Award award = awardDao.getAwardSequenceNumberAndSeqStatusByAwardId(awardId);
		List<AwardAmountInfo> awardAmountInfos = datesAndAmountService.getAwardAmountInfoBasedOnParams(awardId, awardNumber, award.getSequenceNumber(), award.getAwardSequenceStatus());
		if (awardAmountInfos != null && !awardAmountInfos.isEmpty()) {
			awardAmountInfos = awardAmountInfos.stream()
			  .sorted(Comparator.comparing(AwardAmountInfo::getAwardAmountInfoId).reversed())
			  .collect(Collectors.toList());
			return awardAmountInfos.get(0);
		} else 
			return new AwardAmountInfo();
	}
}

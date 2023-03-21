package com.polus.fibicomp.budget.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetNonPersonDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetRateAndBase;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;

@Service
public class AwardBudgetCopyServiceImpl implements AwardBudgetCopyService {

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Override
	public AwardBudgetHeader createAwardBudgetHeader(AwardVO vo, Award award, AwardBudgetHeader awardBudgetHeader,
			String serviceRequestTypeCode, List<AwardBudgetHeader> awardBudgetHeaderDetail) {
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
		awardBudget.setBudgetTemplateTypeId(awardBudgetHeader.getBudgetTemplateTypeId());
		awardBudget.setAvailableFundType(awardBudgetHeader.getAvailableFundType());
		awardBudget.setOnCampusRates(awardBudgetHeader.getOnCampusRates());
		awardBudget.setOffCampusRates(awardBudgetHeader.getOffCampusRates());
		awardBudget.setCostSharingTypeCode(awardBudgetHeader.getCostSharingTypeCode());
		awardBudget.setFundDisbursementBasisTypeCode(awardBudgetHeader.getFundDisbursementBasisTypeCode());
		if (serviceRequestTypeCode.equals(Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE)) {
			awardBudget.setBudgetStatus(awardBudgetDao.getAwardBudgetStatusById(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS));
			awardBudget.setBudgetStatusCode(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS);
			Integer maxBudgetVersionNumber = awardBudgetDao.maxAwardBudgetVersionNumberByAwardId(awardBudgetHeader.getAwardId());
			awardBudget.setVersionNumber(maxBudgetVersionNumber + 1);
			awardBudget.setSequenceNumber(award.getSequenceNumber());
			awardBudget.setBudgetTypeCode(Constants.AWARD_BUDGET_TYPE_CODE_REBUDGET);
			awardBudget.setBudgetType(awardBudgetDao.getBudgetTypeById(Constants.AWARD_BUDGET_TYPE_CODE_REBUDGET));
			awardBudget.setCreateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudget.setCreateUser(vo.getUpdateUser());
			awardBudget.setCreateUserName(vo.getUserFullName());
			awardBudget.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardBudget.setUpdateUser(vo.getUpdateUser());
			awardBudget.setUpdateUserName(vo.getUserFullName());
			awardBudget.setVirement(BigDecimal.ZERO);
			awardBudget.setInitialAvailableFund(awardBudgetService.calculateAvailableFund(awardBudget.getAwardId(),awardBudget.getAwardNumber(), true,
					awardBudget.getAvailableFundType()));
			awardBudget.setAvailableFund(awardBudget.getInitialAvailableFund().subtract(awardBudget.getTotalCost()));
			awardBudget.setObligatedTotal(awardBudgetService.calculateObligatedTotal(award.getAwardId(), award.getAwardNumber()));
			awardBudget.setAnticipatedTotal(awardBudgetService.calculateAnticipatedAmount(award.getAwardId(), award.getAwardNumber()));
			awardBudget.setTotalCostShare(awardBudgetService.calculateCostShareIncludedInBudget(awardBudget.getAwardId()));
			if (awardBudgetHeaderDetail != null) {
				awardBudgetHeaderDetail.stream().forEach(awardbudgetHeaderData -> {
					awardbudgetHeaderData.setIsLatestVersion(false);
					awardBudgetDao.saveOrUpdateAwardBudgetOverView(awardbudgetHeaderData);
				});
			}
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
			awardBudget.setBudgetStatus(awardBudgetHeader.getBudgetStatus());
			awardBudget.setBudgetStatusCode(awardBudgetHeader.getBudgetStatusCode());
			awardBudget.setVirement(awardBudgetHeader.getVirement());
			awardBudget.setObligatedTotal(awardBudgetHeader.getObligatedTotal());
			awardBudget.setAnticipatedTotal(awardBudgetHeader.getAnticipatedTotal());
			awardBudget.setTotalCostShare(awardBudgetHeader.getTotalCostShare());
		}
		awardBudget.setCumulativeVirement(awardBudgetHeader.getCumulativeVirement());
		awardBudgetHeader.setIsLatestVersion(false);
		awardBudget.setIsLatestVersion(true);
		awardBudgetDao.saveOrUpdateAwardBudgetOverView(awardBudgetHeader);
		awardBudgetDao.saveBudgetHeader(awardBudget);
		copyBudgetPersonWithoutLineItem(awardBudgetHeader.getBudgetId(), awardBudget.getBudgetId());
		AtomicBoolean  isAwardBudgetDetailFound = new AtomicBoolean(false);
		List<AwardBudgetPeriod> awardBudgetPeriod = new ArrayList<>();
		if (awardBudgetHeader.getBudgetPeriods() != null && !awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			List<AwardBudgetPeriod> budgetPeriods = copyAwardBudgetPeriods(award, awardBudgetHeader.getBudgetPeriods(), vo.getUpdateUser(), awardBudget, serviceRequestTypeCode, vo.getOriginalAwardId());
			budgetPeriods.stream().forEach(periods -> {
				periods.setBudgetId(awardBudget.getBudgetId());
				awardBudgetDao.saveOrUpdateAwardBudgetPeriod(periods);
				for (AwardBudgetDetail details : periods.getBudgetDetails()) {
					isAwardBudgetDetailFound.set(true);
					details.setBudgetId(awardBudget.getBudgetId());
					details.setBudgetPeriodId(periods.getBudgetPeriodId());
					awardBudgetDao.saveOrUpdateAwardBudgetLineItem(details);
				}
				awardBudgetPeriod.add(periods);
			});
			awardBudget.getBudgetPeriods().addAll(awardBudgetPeriod);
		}
		if (Boolean.TRUE.equals(isAwardBudgetDetailFound.get()) && serviceRequestTypeCode.equals(Constants.BUDGET_VARIATION_SERVICE_REQUEST_TYPE_CODE)) {
			if (awardBudgetHeader.getBudgetPeriods() != null && !awardBudgetHeader.getBudgetPeriods().isEmpty()) {
				awardBudgetService.fetchAwardBudgetPeriods(awardBudget);
			}
			calculateAwardBudget(awardBudget);
		}
		return awardBudget;
	}

	private void copyBudgetPersonWithoutLineItem(Integer budgetId, Integer newBudgetId) {
		List<AwardBudgetPerson> awardBudgetPersons = awardBudgetDao.copyBudgetPersonWithoutLineItem(budgetId);
		awardBudgetPersons.forEach(awardBudgetPerson -> {
			AwardBudgetPerson newAwardBudgetPerson = new AwardBudgetPerson();
			BeanUtils.copyProperties(awardBudgetPerson, newAwardBudgetPerson);
			newAwardBudgetPerson.setBudgetPersonId(null);
			newAwardBudgetPerson.setBudgetHeaderId(newBudgetId);
			awardBudgetDao.saveOrUpdateAwardBudgetPerson(newAwardBudgetPerson);	
		});
	}

	private AwardBudgetVO calculateAwardBudget(AwardBudgetHeader awardBudgetHeader) {
		List<AwardRates> awardRates = awardBudgetDao.fetchAwardRatesByBudgetId(awardBudgetHeader.getBudgetId());
		if (awardRates != null && !awardRates.isEmpty()) {
			awardBudgetHeader.setAwardRates(awardRates);
		}
		awardBudgetService.calculateAwardBudgetPeriod(awardBudgetHeader);
		return awardBudgetService.calculateAwardBudgetHeader(awardBudgetHeader);
	}

	private List<AwardBudgetPeriod> copyAwardBudgetPeriods(Award copyAward, List<AwardBudgetPeriod> budgetPeriods, String updateUser, AwardBudgetHeader awardBudgetHeader, String serviceRequestTypeCode, Integer originalAwardId) {
		List<AwardBudgetPeriod> awardBudgetPeriods = new ArrayList<>();
		budgetPeriods.stream().forEach(copiedAwardBudgetPeriod -> {
			AwardBudgetPeriod awardBudgetPeriodData = new AwardBudgetPeriod();
			awardBudgetPeriodData.setBudgetId(awardBudgetHeader.getBudgetId());
			if (copiedAwardBudgetPeriod.getVersionNumber() == null) {
				awardBudgetPeriodData.setVersionNumber(1);
			} else {
				awardBudgetPeriodData.setVersionNumber(awardBudgetHeader.getVersionNumber());
			}
			awardBudgetPeriodData.setAwardNumber(copyAward.getAwardNumber());
			awardBudgetPeriodData.setBudgetId(awardBudgetHeader.getBudgetId());
			awardBudgetPeriodData.setBudgetPeriod(copiedAwardBudgetPeriod.getBudgetPeriod());
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
			awardBudgetPeriodData.setBudgetDetails(copyAwardBudgetDetails(copyAward, copiedAwardBudgetPeriod.getBudgetDetails(), updateUser, awardBudgetHeader, awardBudgetPeriodData, serviceRequestTypeCode, originalAwardId));
			awardBudgetPeriods.add(awardBudgetPeriodData);
		});
		return awardBudgetPeriods;
	}

    private List<AwardBudgetDetail> copyAwardBudgetDetails(Award copyAward, List<AwardBudgetDetail> budgetDetails, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetPeriod awardBudgetPeriodData, String serviceRequestTypeCode, Integer originalAwardId) {
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		budgetDetails.stream().forEach(copiedAwardBudgetDetail -> {
			AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
			awardBudgetDetail.setBudgetPeriodId(awardBudgetPeriodData.getBudgetPeriodId());
			if (copiedAwardBudgetDetail.getVersionNumber() == null) {
				awardBudgetDetail.setVersionNumber(1);
			} else {
				awardBudgetDetail.setVersionNumber(awardBudgetPeriodData.getVersionNumber());
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
				awardBudgetDetail.setBalanceToDate(copiedAwardBudgetDetail.getBalanceToDate());
			} else {
				awardBudgetDetail.setPrevLineItemCost(copiedAwardBudgetDetail.getLineItemCost());
				awardBudgetDetail.setLineItemCost(copiedAwardBudgetDetail.getLineItemCost());
				awardBudgetService.calculateBalanceToDateValue(awardBudgetDetail, awardBudgetHeader.getFundCode());
			}
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
			awardBudgetDetail.setBudgetRateAndBases(copyAwardBudgetRateAndBase(copiedAwardBudgetDetail.getBudgetRateAndBases(), updateUser, awardBudgetHeader, awardBudgetDetail));
			awardBudgetDetail.setPersonsDetails((copyAwardBudgetPersonalDetails(copiedAwardBudgetDetail.getPersonsDetails(), updateUser, awardBudgetHeader.getBudgetId(), awardBudgetDetail)));
			awardBudgetDetail.setNonPersonsDetails(copyAwardBudgetNonPersonalDetails(copiedAwardBudgetDetail.getNonPersonsDetails(), updateUser, awardBudgetDetail));
			awardBudgetDetail.setPreviousAwardBudgetDetailId(copiedAwardBudgetDetail.getBudgetDetailId());
			awardBudgetDao.saveOrUpdateAwardBudgetLineItem(awardBudgetDetail);
			awardBudgetDetail.setBudgetDetailCalcAmounts(copyAwardBudgetDetailCalcAmount(copiedAwardBudgetDetail.getBudgetDetailCalcAmounts(), updateUser, awardBudgetHeader, awardBudgetDetail));
			awardBudgetDetails.add(awardBudgetDetail);
			});
		return awardBudgetDetails;
	}


	private List<AwardBudgetDetailCalcAmount> copyAwardBudgetDetailCalcAmount(List<AwardBudgetDetailCalcAmount> budgetDetailCalcAmounts, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetDetailCalcAmount> newBudgetDetailCalcAmounts = new ArrayList<>();
		budgetDetailCalcAmounts.stream().forEach(awardBudgetDetailCalcAmount -> {
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
			awardBudgetDao.saveOrUpdateAwardBudgetDetailCalcAmount(awardBudgetDetailCalcAmountData);
			newBudgetDetailCalcAmounts.add(awardBudgetDetailCalcAmountData);
		});
		return newBudgetDetailCalcAmounts;
	}

	private List<AwardBudgetRateAndBase> copyAwardBudgetRateAndBase(List<AwardBudgetRateAndBase> budgetRateAndBases, String updateUser, AwardBudgetHeader awardBudgetHeader, AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetRateAndBase> copiedBudgetRateAndBases = new ArrayList<>();
		budgetRateAndBases.stream().forEach(awardBudgetRateAndBase -> {
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
		});
		return copiedBudgetRateAndBases;
	}

	private List<AwardBudgetPersonalDetail> copyAwardBudgetPersonalDetails(
			List<AwardBudgetPersonalDetail> personsDetails, String updateUser, Integer awardBudgetHeaderId,
			AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetPersonalDetail> awardBudgetPersonDetails = new ArrayList<>();
		personsDetails.stream().forEach(awardBudgetPersonalDetail -> {
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
		});
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

	private AwardBudgetPerson prepareAwardBudgetPersons(Integer awardBudgetHeaderId, String updateUser, AwardBudgetPerson awardBudgetPerson) {
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

	private List<AwardBudgetNonPersonDetail> copyAwardBudgetNonPersonalDetails(List<AwardBudgetNonPersonDetail> nonPersonsDetails, String updateUser, AwardBudgetDetail awardBudgetDetail) {
		List<AwardBudgetNonPersonDetail> awardBudgetNonPersonDetails = new ArrayList<>();
		if (nonPersonsDetails != null && !nonPersonsDetails.isEmpty()) {
			nonPersonsDetails.stream().forEach(awardBudgetPersonalDetail -> {
				AwardBudgetNonPersonDetail awardBudgetNonPersonDetail = new AwardBudgetNonPersonDetail();
				awardBudgetNonPersonDetail.setAwardnonPersonBudgetDetail(awardBudgetDetail);
				awardBudgetNonPersonDetail.setDescription(awardBudgetPersonalDetail.getDescription());
				awardBudgetNonPersonDetail.setInternalOrderCode(awardBudgetPersonalDetail.getInternalOrderCode());
				awardBudgetNonPersonDetail.setLineItemCost(awardBudgetPersonalDetail.getLineItemCost());
				awardBudgetNonPersonDetail.setLineItemNumber(awardBudgetPersonalDetail.getLineItemNumber());
				awardBudgetNonPersonDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				awardBudgetNonPersonDetail.setUpdateUser(updateUser);
				awardBudgetNonPersonDetails.add(awardBudgetNonPersonDetail);
			});
		}
		return awardBudgetNonPersonDetails;
	}
}

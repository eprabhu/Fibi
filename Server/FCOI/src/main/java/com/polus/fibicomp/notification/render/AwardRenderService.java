package com.polus.fibicomp.notification.render;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;

@Transactional
@Service
public class AwardRenderService implements EmailRenderService {

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private ServiceRequestDao serviceRequestDao;
	
	@Autowired
	private DatesAndAmountDao datesAndAmountDao;
	
	DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		Integer awardId = Integer.parseInt(moduleItemKey);
		Award award = awardDao.fetchAwardByAwardId(awardId.toString());
		Map<String, String> placeHolder = getAwardPlaceHolder(award);
		return placeHolder;
	}

	public Map<String, String> getAwardPlaceHolder(Award award) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		String link = generateLinkToApplication(award.getAwardId());
		placeHolder.put("{AWARD_NUMBER}", (award.getAwardNumber() != null) ? award.getAwardNumber() + "" : "");
		placeHolder.put("{AWARD_TITLE}", (award.getTitle() != null) ? award.getTitle() + "" : "");
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", (award.getPrincipalInvestigator() != null) ? award.getPrincipalInvestigator() + "" : "");
		placeHolder.put("{DEADLINE_DATE}", (award.getFinalExpirationDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		List<AwardFundingProposal> awardFundingProposals = awardDao.getAwardFundingProposals(award.getAwardId());
		if (!awardFundingProposals.isEmpty()) {
			AwardFundingProposal awardFundingProposal = awardFundingProposals.get(0);
			InstituteProposal instituteProposal = awardFundingProposal.getProposal();
			placeHolder.put("{PROPOSAL_TITLE}", (instituteProposal.getTitle() != null) ? instituteProposal.getTitle() + "" : "");
			placeHolder.put("{GRANT_CALL_NAME}", (instituteProposal.getGrantCall() != null) ? instituteProposal.getGrantCall().getGrantCallName() + "" : "");
		} else {
			placeHolder.put("{GRANT_CALL_NAME}", "");
			placeHolder.put("{PROPOSAL_TITLE}", "");
		}
		placeHolder.put("{LEAD_UNIT}", (award.getLeadUnit() != null) ? award.getLeadUnit().getUnitName() + "" : "");
		placeHolder.put("{LEAD_UNIT_NUMBER}", (award.getLeadUnitNumber() != null) ? award.getLeadUnitNumber() : "");
		placeHolder.put("{AWARD_TYPE}", (award.getAwardType() != null) ? award.getAwardType().getDescription() + "" : "");
		placeHolder.put("{AWARD_START_DATE}", (award.getBeginDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{AWARD_END_DATE}", (award.getFinalExpirationDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{AWARD_FUNDING_AGENCY}", (award.getSponsor() != null) ? award.getSponsor().getSponsorName() + "" : "");
		AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(award.getAwardId());
		if (awardBudgetHeader != null) {
			StringBuffer lineItemTable = new StringBuffer(0);
			createLineItemTable(awardBudgetHeader, lineItemTable);
			placeHolder.put("{LINE_ITEM_TABLE}", lineItemTable.toString());
			placeHolder.put("{FUND_CENTRE}", (award.getFundCenter() != null) ? award.getFundCenter() + "" : "");
			placeHolder.put("{FUND_CODE}", (award.getAccountNumber() != null) ? award.getAccountNumber() + "" : "");
			placeHolder.put("{TOTAL_INDIRECT_COST}", (awardBudgetHeader.getTotalIndirectCost() != null) ? decimalFormat.format(awardBudgetHeader.getTotalIndirectCost()) + "" : "");
		} else {
			placeHolder.put("{LINE_ITEM_TABLE}", "");
			placeHolder.put("{FUND_CENTRE}", "");
			placeHolder.put("{FUND_CODE}", "");
			placeHolder.put("{TOTAL_INDIRECT_COST}", "");
		}
		AwardAmountInfo awardAmountInfo = awardBudgetService.fetchLatestAwardAmountInfo(award.getAwardId(), award.getAwardNumber());
		if (awardAmountInfo != null) {
			BigDecimal totalCost = calculateAwardTotalProjectValue(award.getAwardId(), awardAmountInfo);
			placeHolder.put("{PROJECT_AMOUNT}", (awardAmountInfo.getObliDistributableAmount() != null) ? decimalFormat.format(awardAmountInfo.getObliDistributableAmount()) + "" : "");
			placeHolder.put("{TOTAL_PROJECT_COST}", (totalCost != null) ? decimalFormat.format(totalCost) + "" : "");
			placeHolder.put("{IN_FOREIGN_CURRENCY}", ((awardAmountInfo.getCurrency() != null && awardAmountInfo.getCurrency().getCurrencySymbol() != null ? awardAmountInfo.getCurrency().getCurrencySymbol() : "") + (awardAmountInfo.getTotalCostInCurrency() != null ? decimalFormat.format(awardAmountInfo.getTotalCostInCurrency()) : "")));
			placeHolder.put("{TOTAL_APPROVED_PROJECT_COST}", (awardAmountInfo.getTotalCostInCurrency() != null) ? decimalFormat.format(awardAmountInfo.getTotalCostInCurrency()) + "" : "");
		} else {
			placeHolder.put("{PROJECT_AMOUNT}", "");
			placeHolder.put("{TOTAL_PROJECT_COST}", "");
			placeHolder.put("{IN_FOREIGN_CURRENCY}", "");
			placeHolder.put("{TOTAL_APPROVED_PROJECT_COST}", "");
		}
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{SPONSOR_AWARD_NUMBER}",(award.getSponsorAwardNumber() != null) ? award.getSponsorAwardNumber() + "" : "");
		placeHolder.put("{PROJECT_TITLE}", (award.getTitle() != null) ? award.getTitle() + "" : "");
		placeHolder.put("{PROJECT_ID}", (award.getAwardNumber() != null) ? award.getAwardNumber() + "" : "");
        placeHolder.put("{ACCOUNT_NUMBER}", (award.getAccountNumber() != null) ? award.getAccountNumber() + "" : "");
        placeHolder.put("{SERVICE_REQUEST_TYPE}", (award.getServiceRequestType() != null ? award.getServiceRequestType().getDescription() + "" :""));
		placeHolder.put("{PRIME_SPONSOR}", (award.getPrimeSponsor() != null ? award.getPrimeSponsor().getSponsorName() + "" :""));
        placeholderForServiceRequestType(placeHolder, award);
        placeholderForServiceRequest(award.getAwardVariationTypeCode(), award.getAwardId().toString(), placeHolder);
        return placeHolder;
	}

	private void placeholderForServiceRequest(String variationTypeCode, String awardId, Map<String, String> placeHolder) {
		if (variationTypeCode != null) {
			ServiceRequest serviceRequest = serviceRequestDao.fetchServiceRequestByOriginatedAward(Constants.AWARD_MODULE_CODE, awardId);
			if (serviceRequest != null) {
				placeHolder.put("{DESCRIPTION}", (serviceRequest.getDescription() != null) ? serviceRequest.getDescription() : "");
				placeHolder.put("{SUBJECT}", (serviceRequest.getSubject() != null) ? serviceRequest.getSubject() : "");
			}
		}
	}

	private void placeholderForServiceRequestType(Map<String, String> placeHolder, Award award) {
		if (award.getServiceRequestType() == null) {
			return;
		}
		List<String> srTypes = new ArrayList<>();
		srTypes.add("9");
		srTypes.add("19");
		srTypes.add("20");
		if (srTypes.contains(award.getServiceRequestType().getTypeCode())) {
			StringBuilder mailContent = new StringBuilder();
			List<AwardManpowerResource> awardManpowerResources = manpowerIntegrationDao.getAwardManpowerResourcesByAwardId(award.getAwardId());
			if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
				mailContent.append("Manpower details are given below <br><br>");
				awardManpowerResources.stream().forEach(resource -> {
					Timestamp allocationStartDate = resource.getChargeStartDate() == null ? resource.getPlanStartDate() : resource.getChargeStartDate();
					Timestamp allocationEndDate = resource.getChargeEndDate() == null ? resource.getPlanEndDate() : resource.getChargeEndDate();
					mailContent.append("Full Name : ").append(resource.getFullName() == null ? "": resource.getFullName()).append("<br>")
					.append("Position Id : ").append(resource.getPositionId()).append("<br>")
					.append("Job Profile Name : ").append(resource.getManpowerJobProfileType() == null ? "" : resource.getManpowerJobProfileType().getDescription()).append("<br>")
					.append("Actual Start Date : ").append(commonService.convertDateFormatBasedOnTimeZone((allocationStartDate.getTime()),Constants.DEFAULT_DATE_FORMAT)).append("<br>")
					.append("Actual End Date : ").append(commonService.convertDateFormatBasedOnTimeZone((allocationEndDate.getTime()),Constants.DEFAULT_DATE_FORMAT)).append("<br>")
					.append("Cost Allocation % : ").append(resource.getCostAllocation()).append("<br>")
					.append("Initial Committed Amount : "+Constants.DOLLAR_SYMBOL +" ").append(resource.getPlannedSalary() == null ? "" : resource.getPlannedSalary()).append("<br>")
					.append("Actual Committed Amount : "+Constants.DOLLAR_SYMBOL +" ").append(resource.getCommittedCost() == null ? "" : resource.getCommittedCost()).append("<br>")
					.append("Upgradation Type : ").append(resource.getManpowerUpgradeType() == null ? "" :resource.getManpowerUpgradeType().getDescription()).append("<br/><br/>");
					});
			}
	        placeHolder.put("{MANPOWER_CONTENT}", mailContent.toString());
		}		
	}

	@Override
	public String getModuleType() {
		return Constants.AWARD_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.AWARD_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AWARD_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

	private void createLineItemTable(AwardBudgetHeader awardBudgetHeader, StringBuffer lineItemTable) {
		if (!awardBudgetHeader.getBudgetPeriods().isEmpty()) {
			AwardBudgetPeriod awardBudgetPeriod = awardBudgetHeader.getBudgetPeriods().get(0);
			List<AwardBudgetDetail> awardBudgetDetails = awardBudgetPeriod.getBudgetDetails();
			if (!awardBudgetDetails.isEmpty()) {
				lineItemTable.append("<table border='1'><tr><th>Budget Line Item</th><th>IO Code</th></tr>");
				for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
					if (awardBudgetDetails.size() != awardBudgetDetails.indexOf(awardBudgetDetail) - 1) {
						if (awardBudgetDetail.getLineItemDescription() != null) {
							lineItemTable.append("<tr><th>" + awardBudgetDetail.getLineItemDescription() + "</th><th>"
									+ ((awardBudgetDetail.getInternalOrderCode() != null)
											? awardBudgetDetail.getInternalOrderCode()
											: "")
									+ "</th></tr>");
						}
					} else {
						lineItemTable.append("<tr><th>"
								+ ((awardBudgetDetail.getLineItemDescription() != null)
										? awardBudgetDetail.getLineItemDescription()
										: "")
								+ "</th><th>"
								+ ((awardBudgetDetail.getInternalOrderCode() != null)
										? awardBudgetDetail.getInternalOrderCode()
										: "")
								+ "</th></tr></table>");
					}
				}
			} else {
				lineItemTable.append("");
			}
		}
	}

	public BigDecimal calculateAwardTotalProjectValue(Integer awardId, AwardAmountInfo awardAmountInfo) {
		BigDecimal sponsorTotalAmount = BigDecimal.ZERO;
		BigDecimal anticipatedTotal = BigDecimal.ZERO;
		BigDecimal instituteTotalAmount = BigDecimal.ZERO;
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		if (awardCostShares != null && !awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				if (awardCostShare.getCommitmentAmount() != null) {
					if (awardCostShare.getSource() != null && awardCostShare.getSource().equals(Constants.SPONSOR)) {
						sponsorTotalAmount = sponsorTotalAmount.add(awardCostShare.getCommitmentAmount());
					} else {
						instituteTotalAmount = instituteTotalAmount.add(awardCostShare.getCommitmentAmount());
					}
				}
			}
		}
		BigDecimal costShareTotal = sponsorTotalAmount.add(instituteTotalAmount);
		if (awardAmountInfo != null && awardAmountInfo.getAnticipatedTotalAmount() != null) {
			anticipatedTotal = awardAmountInfo.getAnticipatedTotalAmount();
			if (awardAmountInfo.getAnticipatedTotalAmount() != null) {
				anticipatedTotal = awardAmountInfo.getAnticipatedTotalAmount();
			}
		}
		return costShareTotal.add(anticipatedTotal);
	}
	
}


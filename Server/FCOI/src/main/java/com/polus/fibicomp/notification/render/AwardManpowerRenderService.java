package com.polus.fibicomp.notification.render;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;

@Transactional
@Service
public class AwardManpowerRenderService implements EmailRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private CommonService commonService;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private ManpowerDao manpowerDao;

	@Override
	public Map<String, String> getPlaceHolderData(String subModuleItemKey) {
		AwardManpowerResource resource = new AwardManpowerResource();
		Award award = new Award();
		if (subModuleItemKey != null) {
			resource = manpowerIntegrationDao.getAwardManpowerResourceById(Integer.parseInt(subModuleItemKey));
			award = manpowerDao.getAwardByResourceId(resource.getManpowerResourceId());
		}
		return getAwardManpowerPlaceHolder(award, resource);
	}

	private Map<String, String> getAwardManpowerPlaceHolder(Award award, AwardManpowerResource resource) {
		Map<String, String> placeHolder = new HashMap<>();
		String link = generateAwardManpowerLinkToApplication(award.getAwardId());
		placeHolder.put("{AWARD_NUMBER}", (award.getAwardNumber() != null) ? award.getAwardNumber() + "" : "");
		placeHolder.put("{AWARD_TITLE}", (award.getTitle() != null) ? award.getTitle() + "" : "");
		placeHolder.put("{ACCOUNT_NUMBER}", (award.getAccountNumber() != null) ? award.getAccountNumber() + "" : "");
		placeHolder.put("{LEAD_UNIT}", (award.getLeadUnit() != null) ? award.getLeadUnit().getUnitName() + "" : "");
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", (award.getPrincipalInvestigator() != null) ? award.getPrincipalInvestigator() + "" : "");
		placeHolder.put("{APPLICATION_URL}", link);
		placeHolder.put("{CANDIDATE_TITLE}", "");
		placeHolder.put("{POSITION_ID}", resource.getPositionId() != null ? resource.getPositionId() : "");
		placeHolder.put("{PERSON_ID}", resource.getPersonId() != null ? resource.getPersonId(): "");
		placeHolder.put("{FULL_NAME}", resource.getFullName() != null ? resource.getFullName() : "");
		placeHolder.put("{COST_ALLOCATION}", resource.getCostAllocation() != null ? resource.getCostAllocation().toString() : "");
		placeHolder.put("{PLAN_START_DATE}", resource.getPlanStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(resource.getPlanStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{PLAN_END_DATE}", resource.getPlanEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(resource.getPlanEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{CHARGE_START_DATE}", resource.getChargeStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(resource.getChargeStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{CHARGE_END_DATE}", resource.getChargeEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(resource.getChargeEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{COMMITTED_COST}", Constants.DOLLAR_SYMBOL + (resource.getCommittedCost() != null ? resource.getCommittedCost().toString() : ""));
		placeHolder.put("{POSITION_OWNED_BY_AWARD}", resource.getPositionOwnedByAward() != null ? resource.getPositionOwnedByAward() : "");
		placeHolder.put("{POSITION_TRIGGER_DATE}", resource.getPositionTriggerDate() != null ? commonService.convertDateFormatBasedOnTimeZone(resource.getPositionTriggerDate().getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{MANPOWER_RESOURCE_TYPE}", resource.getResourceTypeCode() != null ? resource.getManpowerResourceType().getDescription():"");
		placeHolder.put("{JOB_PROFILE_NAME}", resource.getJobProfileTypeCode() != null ? resource.getManpowerJobProfileType().getDescription():"");
		placeHolder.put("{PLANNED_JOB_PROFILE_NAME}", resource.getPlanJobProfileTypeCode() != null ? resource.getManpowerPlanJobProfileType().getDescription():"");
		placeHolder.put("{AWARD_START_DATE}", (award.getBeginDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{AWARD_END_DATE}", (award.getFinalExpirationDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{COMMENTS}", resource.getDescription() != null ? resource.getDescription(): "");
		placeHolder.put("{PLANNED_BASE_SALARY}", resource.getPlannedBaseSalary() != null && !resource.getPlannedBaseSalary().equals(BigDecimal.ZERO) ? resource.getPlannedBaseSalary().toString() : "");
		placeHolder.put("{PLANNED_SALARY}", resource.getPlannedSalary() != null && !resource.getPlannedSalary().equals(BigDecimal.ZERO) ? resource.getPlannedSalary().toString() : "");
		placeHolder.put("{UPGRADE_TYPE}", resource.getUpgradeTypeCode() != null ? resource.getManpowerUpgradeType().getDescription() : "");
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return Constants.AWARD_MODULE_CODE.toString();
	}

	@Override
	public String getSubModuleCode() {
		return Constants.MANPOWER_SUBMODULE_CODE.toString();
	}

	public String generateAwardManpowerLinkToApplication(Integer awardId) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_MANPOWER_PATH + awardId + Constants.APPLICATION_URL_END_TAG;
	}

}
